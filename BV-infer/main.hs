{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.Function ( on )
import Data.List ( nub, sortBy, sort )
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
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import System.Random
import System.Exit
import System.Time ( getClockTime )
import System.Posix.Process ( exitImmediately )

data Problem =
    Problem { problemSize :: Int
            , problemOps :: [String]
            , problemUnaryOps :: [UnaryOp]
            , problemBinaryOps :: [BinaryOp]
            , problemHasFold :: Bool
            , problemHasTFold :: Bool
            , problemHasIf0 :: Bool
            }

problem :: IORef Problem
problem = unsafePerformIO (newIORef $ Problem 0 [] [] [] False False False)

setting :: Int -> [String] -> IO ()
setting size ops = writeIORef problem $
                   Problem { problemSize = size
                           , problemOps = ops
                           , problemUnaryOps = toUOps ops
                           , problemBinaryOps = toBOps ops
                           , problemHasFold = elem "fold" ops
                           , problemHasTFold = elem "tfold" ops
                           , problemHasIf0 = elem "if0" ops
                           }

unsafeGetProblem :: Problem
unsafeGetProblem = unsafePerformIO $ readIORef problem

findProgramWithFold :: [Word64] -> [Word64] -> IO (Maybe (Program WithFold))
findProgramWithFold is os = do
  let ps = genAllProgramWithFold
      ops = filter ("tfold" /=) $ sort $ "fold" : problemOps unsafeGetProblem
  case filter (\p -> sort (opsOfProgram p) == ops) $ filter (\p -> verify p is os) ps of
    p : _ -> return $ Just p
    [] -> error "not found"

findSmallProgramWithFold :: [Word64] -> [Word64] -> IO (Maybe (Program WithFold))
findSmallProgramWithFold is os = do
  let ps = genAllSmallProgramWithFold
  case filter (\p -> verify p is os) ps of
    p : _ -> return $ Just p
    [] -> findProgramWithFold' is os

findProgramWithFold' :: [Word64] -> [Word64] -> IO (Maybe (Program WithFold))
findProgramWithFold' is os = do
  ps <- sample' arbitrary
  case filter (\p -> verify p is os) ps of
    p : _ -> return $ Just p
    [] -> findProgramWithFold' is os

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

findProgramWithoutFold :: [Word64] -> [Word64] -> IO (Maybe (Program WithoutFold))
findProgramWithoutFold is os = do
  let ps = genAllProgramWithoutFold
      ops = sort $ problemOps unsafeGetProblem
  case filter (\p -> sort (opsOfProgram p) == ops) $ filter (\p -> verify p is os) ps of
    p : _ -> return $ Just p
    [] -> error "not found"

findSmallProgramWithoutFold :: [Word64] -> [Word64] -> IO (Maybe (Program WithoutFold))
findSmallProgramWithoutFold is os = do
  let ps = genAllSmallProgramWithoutFold
  case filter (\p -> verify p is os) ps of
    p : _ -> return $ Just p
    [] -> findProgramWithoutFold' is os

findProgramWithoutFold' :: [Word64] -> [Word64] -> IO (Maybe (Program WithoutFold))
findProgramWithoutFold' is os = do
  ps <- sample' arbitrary
  case filter (\p -> verify p is os) ps of
    p : _ -> return $ Just p
    [] -> findProgramWithoutFold' is os

postTrain :: Maybe Int -> Maybe [String] -> IO [Value]
postTrain size ops = do
  let reqJson = object [ "size" .= size
                       , "operators" .= ops
                       ]
  request <- parseUrl "http://icfpc2013.cloudapp.net/train"
  withManager $ \manager -> do
    response <- http request { method = "POST"
                             , queryString = "auth=0164tBAjsBCWQrxfjTfakr5yCCbs8KIurruRhCdivpsH1H"
                             , requestBody = RequestBodyLBS $ encode reqJson
                             , responseTimeout = Just 20000000
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
                             , responseTimeout = Just 20000000
                             } manager
    lbs <- responseBody response $$+- sinkLbs
    case decode' lbs of
      Just resJsons -> return [ resJson
                              | resJson <- resJsons
                              -- , let Just operators = resJson ..: "operators"
                              -- , ("fold" :: String) `notElem` operators -- とりあえず無視
                              -- , ("tfold" :: String) `elem` operators -- ターゲット絞る
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
                             , responseTimeout = Just 20000000
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
                             , responseTimeout = Just 20000000
                             } manager
    lbs <- responseBody response $$+- sinkLbs
    case decode' lbs of
      Just resJson -> return resJson
      Nothing -> error "error: invalid json"

(..:) ::  FromJSON a => Value -> Text -> Maybe a
v ..: t = parseMaybe (withObject "" (.: t)) v

main :: IO ()
main = do
  -- probs <- postTrain Nothing (Just ["fold"])
  probs <- postMyProblems
  let prob = head $ sortBy (compare `on` (\x -> x ..: "size" :: Maybe Int)) probs
  print prob
  let Just pid = prob ..: "id"
      Just size = prob ..: "size"
      Just ops = prob ..: "operators"
  setting size ops
  let fibs = [0,1,2,3,5,8,13,21,34,55 :: Int]
      bits = [ 0
             , complement 0
             , foldr ((+).(2^)) 0 fibs
             , foldr ((+).(2^)) 0 (reverse fibs)
             , 0x5555555555555555 -- 0101
             , 0xAAAAAAAAAAAAAAAA -- 1010
             , 0x3333333333333333 -- 0011
             , 0xCCCCCCCCCCCCCCCC -- 1100
             , 0x0F0F0F0F0F0F0F0F -- 00001111
             , 0xF0F0F0F0F0F0F0F0 -- 00001111
             , 0x00FF00FF00FF00FF
             , 0xFF00FF00FF00FF00
             , 0x0000FFFF0000FFFF
             , 0xFFFF0000FFFF0000
             , 0x00000000FFFFFFFF
             , 0xFFFFFFFF00000000
             ] ++
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
  tryInferProgram pid ops is os

timeout :: IO (Maybe a)
timeout = do
  getClockTime >>= print
  threadDelay (5 * 60 * 1000 * 1000)
  getClockTime >>= print
  putStrLn "timeout"
  return Nothing

tryInferProgram :: String -> [String] -> [Word64] -> [Word64] -> IO ()
tryInferProgram pid ops is os = do
  result <- inferProgram pid ops is os
  print result
  case (result ..: "status" :: Maybe String) of
    Just "win" -> exitImmediately ExitSuccess
    Just "mismatch" -> do
      threadDelay 5000000
      let Just [i,o,_] = result ..: "values"
      tryInferProgram pid ops (read i:is) (read o:os)
    _          -> exitImmediately (ExitFailure 1)

inferProgram :: String -> [String] -> [Word64] -> [Word64] -> IO Value
inferProgram pid ops is os | elem "fold" ops || elem "tfold" ops = do
  (_, manswer) <- mapM async (timeout :
                              findSmallProgramWithFold is os :
                              -- findProgramWithFold is os :
                              replicate 4 (findProgramWithFold' is os)) >>= waitAnyCancel
  case manswer of
    Just answer -> do
      print answer
      postGuess pid answer
    Nothing -> do
      exitImmediately ExitSuccess
      return undefined
inferProgram pid _ops is os = do
  (_, manswer) <- mapM async (timeout :
                              findSmallProgramWithoutFold is os :
                              -- findProgramWithoutFold is os :
                              replicate 4 (findProgramWithoutFold' is os)) >>= waitAnyCancel
  case manswer of
    Just answer -> do
      print answer
      postGuess pid answer
    Nothing -> do
      exitImmediately ExitSuccess
      return undefined

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
                   [ cost4 | n > 3, problemHasIf0 unsafeGetProblem ]
      cost1 = [ return ExpZero
              , return ExpOne
              , ExpId . Id <$> oneof (map return (if shadowing then [0..1] else [1..2]))
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
                   [ cost4 | n > 3, problemHasIf0 unsafeGetProblem ]
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
genExpSizeWithFold n = oneof $ concat candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = [ cost2 | n > 6, not $ null uops ] ++
                   [ cost3 | n > 7, not $ null bops ] ++
                   [ cost4 | n > 8, problemHasIf0 unsafeGetProblem ] ++
                   [ cost5 | n > 4 ]
      cost2 = [ ExpUOp <$> (oneof $ map return uops) <*> genExpSizeWithFold (n-1) ]
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
      cost5 = [ do e0 <- genExpSizeOutFold (n-4)
                   e1 <- genExpSizeOutFold (n-3-sizeOfExp e0)
                   e2 <- genExpSizeInFold False (n-2-sizeOfExp e0-sizeOfExp e1)
                   return $ ExpFold e0 e1 (Id 1) (Id 2) e2
              ]

genExpSizeWithTFold :: Int -> Gen (Exp OutFold WithFold)
genExpSizeWithTFold n = oneof candidates
    where
      candidates = [ cost5 | n > 4 ]
      cost5 = do
        e2 <- genExpSizeInFold True (n-4)
        return $ ExpFold (ExpId (Id 0)) ExpZero (Id 0) (Id 1) e2

instance Arbitrary (Exp OutFold WithFold) where
    arbitrary = gen (size - 1)
        where
          size = problemSize unsafeGetProblem
          gen = if problemHasTFold unsafeGetProblem
                   then genExpSizeWithTFold
                   else genExpSizeWithFold

instance Arbitrary (Exp OutFold WithoutFold) where
    arbitrary = genExpSizeOutFold (size - 1)
        where
          size = problemSize unsafeGetProblem

genAllProgramWithFold :: [Program WithFold]
genAllProgramWithFold = Program (Id 0) <$> gen (size - 1)
    where
      size = problemSize unsafeGetProblem
      gen = if problemHasTFold unsafeGetProblem
               then genAllExpSizeWithTFold
               else genAllExpSizeWithFold

genAllSmallProgramWithFold :: [Program WithFold]
genAllSmallProgramWithFold = Program (Id 0) <$> gen
    where
      size = problemSize unsafeGetProblem
      gen = if problemHasTFold unsafeGetProblem
               then [1..size - 1] >>= genAllExpSizeWithTFold
               else ([1..size - 1] >>= genAllExpSizeWithFold') ++
                    ([1..size - 1] >>= genAllExpSizeWithFold)

genAllProgramWithoutFold :: [Program WithoutFold]
genAllProgramWithoutFold = Program (Id 0) <$> genAllExpSizeOutFold (size - 1)
    where
      size = problemSize unsafeGetProblem

genAllSmallProgramWithoutFold :: [Program WithoutFold]
genAllSmallProgramWithoutFold = Program (Id 0) <$> ([1..size - 1] >>= genAllExpSizeOutFold)
    where
      size = problemSize unsafeGetProblem

genAllExpSizeWithFold :: Int -> [Exp OutFold WithFold]
genAllExpSizeWithFold n = concat candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = [ cost2 | n > 6, not $ null uops ] ++
                   [ cost3 | n > 7, not $ null bops ] ++
                   [ cost4 | n > 8, problemHasIf0 unsafeGetProblem ] ++
                   [ cost5 | n > 4 ]
      cost2 = [ ExpUOp op e0
              | op <- uops
              , e0 <- genAllExpSizeWithFold (n-1)
              , null [ True | op == UnaryOpNot, ExpUOp op' _ <- [e0], op' == UnaryOpNot ] -- not . not はidなので意味無い
              ]
      cost3 = [ ExpBOp op e0 e1
              | op <- bops
              , (n0,n1) <- [ (n0, n1) | n0 <- [1..n], let n1 = n-1-n0, 0 < n1 ]
              , e0 <- genAllExpSizeWithFold n0
              , e1 <- genAllExpSizeOutFold n1
              , not $ and [ op == BinaryOpOr, e1 == ExpZero ] -- or 0 は id なので意味無い
              , not $ and [ op == BinaryOpAnd, e1 == ExpZero ] -- and 0 は 0 なので意味無い
              ] ++
              [ ExpBOp op e0 e1
              | op <- bops
              , (n0,n1) <- [ (n0, n1) | n0 <- [1..n], let n1 = n-1-n0, 0 < n1 ]
              , e1 <- genAllExpSizeWithFold n0
              , e0 <- genAllExpSizeOutFold n1
              , not $ and [ op == BinaryOpOr, e0 == ExpZero ] -- or 0 は id なので意味無い
              , not $ and [ op == BinaryOpAnd, e0 == ExpZero ] -- and 0 は 0 なので意味無い
              ]
      cost4 = [ ExpIf0 e0 e1 e2
              | (n0,n1,n2) <- [ (n0, n1, n2)
                              | n0 <- [1..n]
                              , n1 <- [1..n-n0]
                              , let n2=n-1-n0-n1, 0 < n2
                              ]
              , e0 <- genAllExpSizeWithFold n0
              , e1 <- genAllExpSizeOutFold n1
              , e2 <- genAllExpSizeOutFold n2
              ] ++
              [ ExpIf0 e0 e1 e2
              | (n0,n1,n2) <- [ (n0, n1, n2)
                              | n0 <- [1..n]
                              , n1 <- [1..n-n0]
                              , let n2=n-1-n0-n1, 0 < n2
                              ]
              , e1 <- genAllExpSizeWithFold n0
              , e2 <- genAllExpSizeOutFold n1
              , e0 <- genAllExpSizeOutFold n2
              , not $ and [ e0 == ExpZero ] -- if0 0 は意味無い
              , not $ and [ e0 == ExpOne ] -- if0 1 は意味無い
              ] ++
              [ ExpIf0 e0 e1 e2
              | (n0,n1,n2) <- [ (n0, n1, n2)
                              | n0 <- [1..n]
                              , n1 <- [1..n-n0]
                              , let n2=n-1-n0-n1, 0 < n2
                              ]
              , e2 <- genAllExpSizeWithFold n0
              , e0 <- genAllExpSizeOutFold n1
              , e1 <- genAllExpSizeOutFold n2
              , not $ and [ e0 == ExpZero ] -- if0 0 は意味無い
              , not $ and [ e0 == ExpOne ] -- if0 1 は意味無い
              ]
      cost5 = do
        (n0,n1,n2) <- [ (n0, n1, n2)
                      | n0 <- [1..n]
                      , n1 <- [1..n-n0]
                      , let n2=n-2-n0-n1, 0 < n2
                      ]
        e0 <- genAllExpSizeOutFold n0
        e1 <- genAllExpSizeOutFold n1
        e2 <- genAllExpSizeInFold False n2
        return $ ExpFold e0 e1 (Id 1) (Id 2) e2

genAllExpSizeWithFold' :: Int -> [Exp OutFold WithFold]
genAllExpSizeWithFold' n = concat [ cost5 | n > 4 ]
    where
      cost5 = do
        (n0,n1,n2) <- [ (n0, n1, n2)
                      | n0 <- [1..n]
                      , n1 <- [1..n-n0]
                      , let n2=n-2-n0-n1, 0 < n2
                      ]
        e0 <- genAllExpSizeOutFold n0
        e1 <- genAllExpSizeOutFold n1
        e2 <- genAllExpSizeInFold False n2
        return $ ExpFold e0 e1 (Id 1) (Id 2) e2

genAllExpSizeWithTFold :: Int -> [Exp OutFold WithFold]
genAllExpSizeWithTFold n = do
  e2 <- genAllExpSizeInFold True (n-4)
  return $ ExpFold (ExpId (Id 0)) ExpZero (Id 0) (Id 1) e2

genAllExpSizeInFold :: Bool -> Int -> [Exp InFold WithoutFold]
genAllExpSizeInFold shadowing n = concat candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = [ cost1 ] ++
                   [ cost2 | n > 1, not $ null uops ] ++
                   [ cost3 | n > 2, not $ null bops ] ++
                   [ cost4 | n > 3, problemHasIf0 unsafeGetProblem ]
      cost1 = [ ExpZero
              , ExpOne
              ] ++ map (ExpId . Id) (if shadowing then [0..1] else [1..2])
      cost2 = [ ExpUOp op e0
              | op <- uops
              , e0 <- genAllExpSizeInFold shadowing (n-1)
              , null [ True | op == UnaryOpNot, ExpUOp op' _ <- [e0], op' == UnaryOpNot ] -- not . not はidなので意味無い
              ]
      cost3 = [ ExpBOp op e0 e1
              | op <- bops
              , (n0,n1) <- [ (n0, n1) | n0 <- [1..n], let n1 = n-1-n0, 0 < n1 ]
              , e0 <- genAllExpSizeInFold shadowing n0
              , not $ and [ op == BinaryOpOr, e0 == ExpZero ] -- or 0 は id なので意味無い
              , not $ and [ op == BinaryOpAnd, e0 == ExpZero ] -- and 0 は 0 なので意味無い
              , e1 <- genAllExpSizeInFold shadowing n1
              , not $ and [ op == BinaryOpOr, e1 == ExpZero ] -- or 0 は id なので意味無い
              , not $ and [ op == BinaryOpAnd, e1 == ExpZero ] -- and 0 は 0 なので意味無い
              , not $ and [ op == BinaryOpXor, e0 == e1 ] -- xor a a は 0 なので意味無い
              , not $ and [ op == BinaryOpAnd, e0 == e1 ] -- and a a は a なので意味無い
              , not $ and [ op == BinaryOpOr, e0 == e1 ] -- or a a は a なので意味無い
              ]
      cost4 = [ ExpIf0 e0 e1 e2
              | (n0,n1,n2) <- [ (n0, n1, n2)
                              | n0 <- [1..n]
                              , n1 <- [1..n-n0]
                              , let n2=n-1-n0-n1, 0 < n2
                              ]
              , e0 <- genAllExpSizeInFold shadowing n0
              , e0 /= ExpZero -- if0 0 は意味無い
              , e0 /= ExpOne -- if0 1 は意味無い
              , e1 <- genAllExpSizeInFold shadowing n1
              , e2 <- genAllExpSizeInFold shadowing n2
              ]

genAllExpSizeOutFold :: Int -> [Exp OutFold WithoutFold]
genAllExpSizeOutFold n = concat candidates
    where
      uops = problemUnaryOps unsafeGetProblem
      bops = problemBinaryOps unsafeGetProblem
      candidates = [ cost1 ] ++
                   [ cost2 | n > 1, not $ null uops ] ++
                   [ cost3 | n > 2, not $ null bops ] ++
                   [ cost4 | n > 3, problemHasIf0 unsafeGetProblem ]
      cost1 = [ ExpZero
              , ExpOne
              , ExpId (Id 0)
              ]
      cost2 = [ ExpUOp op e0
              | op <- uops
              , e0 <- genAllExpSizeOutFold (n-1)
              , null [ True | op == UnaryOpNot, ExpUOp op' _ <- [e0], op' == UnaryOpNot ] -- not . not はidなので意味無い
              ]
      cost3 = [ ExpBOp op e0 e1
              | op <- bops
              , (n0,n1) <- [ (n0, n1) | n0 <- [1..n], let n1 = n-1-n0, 0 < n1 ]
              , e0 <- genAllExpSizeOutFold n0
              , not $ and [ op == BinaryOpOr, e0 == ExpZero ] -- or 0 は id なので意味無い
              , not $ and [ op == BinaryOpAnd, e0 == ExpZero ] -- and 0 は 0 なので意味無い
              , e1 <- genAllExpSizeOutFold n1
              , not $ and [ op == BinaryOpOr, e1 == ExpZero ] -- or 0 は id なので意味無い
              , not $ and [ op == BinaryOpAnd, e1 == ExpZero ] -- and 0 は 0 なので意味無い
              , not $ and [ op == BinaryOpXor, e0 == e1 ] -- xor a a は 0 なので意味無い
              , not $ and [ op == BinaryOpAnd, e0 == e1 ] -- and a a は a なので意味無い
              , not $ and [ op == BinaryOpOr, e0 == e1 ] -- or a a は a なので意味無い
              ]
      cost4 = [ ExpIf0 e0 e1 e2
              | (n0,n1,n2) <- [ (n0, n1, n2)
                              | n0 <- [1..n]
                              , n1 <- [1..n-n0]
                              , let n2=n-1-n0-n1, 0 < n2
                              ]
              , e0 <- genAllExpSizeOutFold n0
              , not $ and [ e0 == ExpZero ] -- if0 0 は意味無い
              , not $ and [ e0 == ExpOne ] -- if0 1 は意味無い
              , e1 <- genAllExpSizeOutFold n1
              , e2 <- genAllExpSizeOutFold n2
              ]
