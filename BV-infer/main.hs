{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.Function ( on )
import Data.List ( nub, sortBy )
import Data.Word
import Data.IORef
import Data.Text ( Text )
import Test.QuickCheck
import Language.BV.Syntax
import Language.BV.Eval
import System.IO.Unsafe
import Data.Conduit ( ($$+-) )
import Data.Conduit.Binary (sinkLbs)
import Network.HTTP.Conduit
import Numeric
import Control.Concurrent.Async
import System.Random

data Problem =
    Problem { problemSize :: Int
            , problemUnaryOps :: [UnaryOp]
            , problemBinaryOps :: [BinaryOp]
            , problemHasFold :: Bool
            , problemHasTFold :: Bool
            }

problem :: IORef Problem
problem = unsafePerformIO (newIORef $ Problem 0 [] [] False False)

setting :: Int -> [String] -> IO ()
setting size ops = writeIORef problem $
                   Problem { problemSize = size
                           , problemUnaryOps = toUOps ops
                           , problemBinaryOps = toBOps ops
                           , problemHasFold = elem "fold" ops
                           , problemHasTFold = elem "tfold" ops
                           }

unsafeGetProblem :: Problem
unsafeGetProblem = unsafePerformIO $ readIORef problem

findProgramWithFold :: [Word64] -> [Word64] -> IO (Program WithFold)
findProgramWithFold is os = do
  ps <- sample' arbitrary
  case [ p | p <- ps, verify p is os ] of
    p : _ -> return p
    [] -> findProgramWithFold is os

toUOp :: String -> Maybe UnaryOp
toUOp "not" = Just UnaryOpNot
toUOp "shl1" = Just UnaryOpShl1
toUOp "shr1" = Just UnaryOpShr1
toUOp "shr4" = Just UnaryOpShr4
toUOp "shr16" = Just UnaryOpShr16
toUOp _ = Nothing

toUOps :: [String] -> [UnaryOp]
toUOps ops = [ op | Just op <- map toUOp ops ]

toBOp :: String -> Maybe BinaryOp
toBOp "and" = Just BinaryOpAnd
toBOp "or" = Just BinaryOpOr
toBOp "xor" = Just BinaryOpXor
toBOp "plus" = Just BinaryOpPlus
toBOp _ = Nothing

toBOps :: [String] -> [BinaryOp]
toBOps ops = [ op | Just op <- map toBOp ops ]

findProgramWithoutFold :: [Word64] -> [Word64] -> IO (Program WithoutFold)
findProgramWithoutFold is os = do
  ps <- sample' arbitrary
  case [ p | p <- ps, verify p is os ] of
    p : _ -> return p
    [] -> findProgramWithoutFold is os

postTrain :: Maybe Int -> Maybe String -> IO [Value]
postTrain size ops = do
  let reqJson = object [ "size" .= size
                       , "operators" .= ops
                       ]
  request <- parseUrl "http://icfpc2013.cloudapp.net/train"
  withManager $ \manager -> do
    response <- http request { method = "POST"
                             , queryString = "auth=0164tBAjsBCWQrxfjTfakr5yCCbs8KIurruRhCdivpsH1H"
                             , requestBody = RequestBodyLBS $ encode reqJson
                             , responseTimeout = Just 10000000
                             } manager
    lbs <- responseBody response $$+- sinkLbs
    case decode' lbs of
      Just resJson -> return [resJson]
      Nothing -> error "error: invalid json"

postMyProblems :: IO [Value]
postMyProblems = do
  request <- parseUrl "http://icfpc2013.cloudapp.net/myproblems"
  withManager $ \manager -> do
    response <- http request { method = "POST"
                             , queryString = "auth=0164tBAjsBCWQrxfjTfakr5yCCbs8KIurruRhCdivpsH1H"
                             , responseTimeout = Just 10000000
                             } manager
    lbs <- responseBody response $$+- sinkLbs
    case decode' lbs of
      Just resJsons -> return [ resJson
                              | resJson <- resJsons
                              , let solved = resJson ..: "solved"
                              , Just True /= solved
                              , let timeLeft = resJson ..: "timeLeft"
                              , Just (0 :: Double) < timeLeft || Nothing == timeLeft
                              ]
      Nothing -> error "error: invalid json"

postEval :: String -> [Word64] -> IO Value
postEval pid args = do
  let reqJson = object [ "id" .= pid
                       , "arguments" .= map (\x -> "0x" ++ showHex x "") args
                       ]
  request <- parseUrl "http://icfpc2013.cloudapp.net/eval"
  withManager $ \manager -> do
    response <- http request { method = "POST"
                             , queryString = "auth=0164tBAjsBCWQrxfjTfakr5yCCbs8KIurruRhCdivpsH1H"
                             , requestBody = RequestBodyLBS $ encode reqJson
                             , responseTimeout = Just 10000000
                             } manager
    lbs <- responseBody response $$+- sinkLbs
    case decode' lbs of
      Just resJson -> return resJson
      Nothing -> error "error: invalid json"

postGuess :: AtMostOneOccurrenceOfFold a => String -> Program a -> IO Value
postGuess pid program = do
  let reqJson = object [ "id" .= pid
                       , "program" .= show program
                       ]
  request <- parseUrl "http://icfpc2013.cloudapp.net/guess"
  withManager $ \manager -> do
    response <- http request { method = "POST"
                             , queryString = "auth=0164tBAjsBCWQrxfjTfakr5yCCbs8KIurruRhCdivpsH1H"
                             , requestBody = RequestBodyLBS $ encode reqJson
                             , responseTimeout = Just 10000000
                             } manager
    lbs <- responseBody response $$+- sinkLbs
    case decode' lbs of
      Just resJson -> return resJson
      Nothing -> error "error: invalid json"

(..:) ::  FromJSON a => Value -> Text -> Maybe a
v ..: t = parseMaybe (withObject "" (.: t)) v

main :: IO ()
main = do
  -- probs <- postTrain Nothing Nothing
  probs <- postMyProblems
  let prob = head $ sortBy (compare `on` (\x -> x ..: "size" :: Maybe Int)) probs
  print prob
  let Just pid = prob ..: "id"
      Just size = prob ..: "size"
      Just ops = prob ..: "operators"
  setting size ops
  let fibs = [0,1,2,3,5,8,13,21,34,55 :: Int]
      bits = [0, complement 0] ++
             [foldr ((+).(2^)) 0 fibs] ++
             [foldr ((+).(2^)) 0 (reverse fibs)] ++
             map (2^) [0..63 :: Int] ++
             map ((1+).(2^)) [1..63 :: Int] ++
             map ((2^(63 :: Int)+).(2^)) [0..62 :: Int]
      is0 = take 256 $ nub $ bits
  evalres0 <- postEval pid is0
  let Just os0' = evalres0 ..: "outputs"
      os0 = map read os0'
  is1 <- replicateM 256 (getStdRandom random)
  evalres1 <- postEval pid is1
  let Just os1' = evalres1 ..: "outputs"
      os1 = map read os1'
  let is = is0 ++ is1
      os = os0 ++ os1
  if elem "fold" ops || elem "tfold" ops
     then do
       (_, answer) <- mapM async (replicate 4 $ findProgramWithFold is os) >>= waitAny
       print answer
       postGuess pid answer >>= print
     else do
       (_, answer) <- mapM async (replicate 4 $ findProgramWithoutFold is os) >>= waitAny
       print answer
       postGuess pid answer >>= print
  return ()

verify :: AtMostOneOccurrenceOfFold fold => Program fold -> [Word64] -> [Word64] -> Bool
verify prog is os = and [ p i == o | (i, o) <- zip is os]
    where
      p = evalProgram prog

instance Arbitrary (Program WithFold) where
    arbitrary = Program (Id 0) <$> arbitrary

instance Arbitrary (Program WithoutFold) where
    arbitrary = Program (Id 0) <$> arbitrary

genExpSizeInFold :: Bool -> Int -> Gen (Exp InFold WithoutFold)
genExpSizeInFold shadowing n = oneof candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = cost1 ++
                   [ cost2 | n > 1, not $ null uops ] ++
                   [ cost3 | n > 2, not $ null bops ] ++
                   [ cost4 | n > 3 ]
      cost1 = [ return ExpZero
              , return ExpOne
              , ExpId . Id <$> oneof (map return [0..if shadowing then 1 else 2])
              ]
      cost2 = ExpUOp <$> (oneof $ map return uops) <*> genExpSizeInFold shadowing (n-1)
      cost3 = do
        op <- oneof $ map return bops
        e0 <- genExpSizeInFold shadowing (n-2)
        e1 <- genExpSizeInFold shadowing (n-1-sizeOfExp e0)
        return $ ExpBOp op e0 e1
      cost4 = do
        e0 <- genExpSizeInFold shadowing (n-3)
        e1 <- genExpSizeInFold shadowing (n-2-sizeOfExp e0)
        e2 <- genExpSizeInFold shadowing (n-1-sizeOfExp e0-sizeOfExp e1)
        return $ ExpIf0 e0 e1 e2

genExpSizeOutFold :: Int -> Gen (Exp OutFold WithoutFold)
genExpSizeOutFold n = oneof candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = cost1 ++
                   [ cost2 | n > 1, not $ null uops ] ++
                   [ cost3 | n > 2, not $ null bops ] ++
                   [ cost4 | n > 3 ]
      cost1 = [ return ExpZero
              , return ExpOne
              , return $ ExpId (Id 0)
              ]
      cost2 = ExpUOp <$> (oneof $ map return uops) <*> genExpSizeOutFold (n-1)
      cost3 = do
        op <- oneof $ map return bops
        e0 <- genExpSizeOutFold (n-2)
        e1 <- genExpSizeOutFold (n-1-sizeOfExp e0)
        return $ ExpBOp op e0 e1
      cost4 = do
        e0 <- genExpSizeOutFold (n-3)
        e1 <- genExpSizeOutFold (n-2-sizeOfExp e0)
        e2 <- genExpSizeOutFold (n-1-sizeOfExp e0-sizeOfExp e1)
        return $ ExpIf0 e0 e1 e2

genExpSizeWithFold :: Int -> Gen (Exp OutFold WithFold)
genExpSizeWithFold n = oneof candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = [ cost2 | n > 6, not $ null uops ] ++
                   filter (const (n > 7 && not (null bops))) cost3 ++
                   filter (const (n > 8)) cost4 ++
                   [ cost5 | n > 4 ]
      cost2 = ExpUOp <$> (oneof $ map return uops) <*> genExpSizeWithFold (n-1)
      cost3 = [ do op <- oneof $ map return bops
                   e0 <- genExpSizeWithFold (n-2)
                   e1 <- genExpSizeOutFold (n-1-sizeOfExp e0)
                   return $ ExpBOp op e0 e1
              , do op <- oneof $ map return bops
                   e1 <- genExpSizeWithFold (n-2)
                   e0 <- genExpSizeOutFold (n-1-sizeOfExp e1)
                   return $ ExpBOp op e0 e1
              ]
      cost4 = [ do e0 <- genExpSizeWithFold (n-3)
                   e1 <- genExpSizeOutFold (n-2-sizeOfExp e0)
                   e2 <- genExpSizeOutFold (n-1-sizeOfExp e0-sizeOfExp e1)
                   return $ ExpIf0 e0 e1 e2
              , do e1 <- genExpSizeWithFold (n-3)
                   e2 <- genExpSizeOutFold (n-2-sizeOfExp e1)
                   e0 <- genExpSizeOutFold (n-1-sizeOfExp e1-sizeOfExp e2)
                   return $ ExpIf0 e0 e1 e2
              , do e2 <- genExpSizeWithFold (n-3)
                   e0 <- genExpSizeOutFold (n-2-sizeOfExp e2)
                   e1 <- genExpSizeOutFold (n-1-sizeOfExp e2-sizeOfExp e0)
                   return $ ExpIf0 e0 e1 e2
              ]
      cost5 = do
        e0 <- genExpSizeOutFold (n-4)
        e1 <- genExpSizeOutFold (n-3-sizeOfExp e0)
        e2 <- genExpSizeInFold False (n-2-sizeOfExp e0-sizeOfExp e1)
        return $ ExpFold e0 e1 (Id 1) (Id 2) e2

genExpSizeWithTFold :: Int -> Gen (Exp OutFold WithFold)
genExpSizeWithTFold n = oneof candidates
    where
      candidates = [ cost5 | n > 4 ]
      cost5 = do
        e2 <- genExpSizeInFold True (n-4)
        return $ ExpFold (ExpId (Id 0)) ExpZero (Id 0) (Id 1) e2

instance Arbitrary (Exp OutFold WithFold) where
    arbitrary = sized $ gen . max (problemSize unsafeGetProblem - 1)
        where
          gen = if problemHasTFold unsafeGetProblem
                   then genExpSizeWithTFold
                   else genExpSizeWithFold

instance Arbitrary (Exp OutFold WithoutFold) where
    arbitrary = sized $ genExpSizeOutFold . max (problemSize unsafeGetProblem - 1)
