{-# LANGUAGE GADTs #-}
module Language.BV.Eval where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.IntMap
import Data.STRef
import Data.Word ( Word64 )
import Language.BV.Syntax

evalProgram :: AtMostOneOccurrenceOfFold fold => Program fold -> Word64 -> Word64
evalProgram (Program (Id n) expr) x = runST $ do
                                       env <- newSTRef (singleton n x)
                                       evalExp env expr

type Env s = STRef s (IntMap Word64)

evalExp :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Env s -> Exp io fold -> ST s Word64
evalExp _   ExpZero = return 0
evalExp _   ExpOne = return 1
evalExp env (ExpId (Id n)) = (! n) <$> readSTRef env
evalExp env (ExpIf0 e0 e1 e2) = do
  p <- evalExp env e0
  if p == 0 then evalExp env e1 else evalExp env e2
evalExp env (ExpFold e0 e1 (Id n0) (Id n1) e2) = do
  x <- evalExp env e0
  z <- evalExp env e1
  let xs = [ shiftR x (8 * n) .&. 0xFF | n <- [0..7] ]
  foldM (\a b -> modifySTRef' env (insert n0 b . insert n1 a) >> evalExp env e2) z xs
evalExp env (ExpUOp op e0) = evalUOp op <$> evalExp env e0
evalExp env (ExpBOp op e0 e1) = evalBOp op <$> evalExp env e0 <*> evalExp env e1

evalUOp :: UnaryOp -> Word64 -> Word64
evalUOp UnaryOpNot = complement
evalUOp UnaryOpShl1 = flip shiftL 1
evalUOp UnaryOpShr1 = flip shiftR 1
evalUOp UnaryOpShr4 = flip shiftR 4
evalUOp UnaryOpShr16 = flip shiftR 16

evalBOp :: BinaryOp -> Word64 -> Word64 -> Word64
evalBOp BinaryOpAnd = (.&.)
evalBOp BinaryOpOr = (.|.)
evalBOp BinaryOpXor = xor
evalBOp BinaryOpPlus = (+)
