{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Language.BV.Syntax where

import Data.List ( nub )

-- | program    P ::= "(" "lambda" "(" id ")" e ")"
data Program fold = Program Id (Exp OutFold fold)

instance AtMostOneOccurrenceOfFold fold => Show (Program fold) where
    show (Program ident expr) = "(lambda (" ++ shows ident ") " ++ shows expr ")"

-- A valid program P contains at most one occurrence of "fold".
class AtMostOneOccurrenceOfFold fold where

data WithFold
instance AtMostOneOccurrenceOfFold WithFold where

data WithoutFold
instance AtMostOneOccurrenceOfFold WithoutFold where

type family a :+: b :: *
type instance WithoutFold :+: WithoutFold = WithoutFold
type instance WithFold    :+: WithoutFold = WithFold
type instance WithoutFold :+: WithFold    = WithFold

-- In/Out fold
class FoldInOut inout where

data InFold
instance FoldInOut InFold where

data OutFold
instance FoldInOut OutFold where


data Exp inout fold where
    ExpZero :: FoldInOut io => Exp io WithoutFold
    ExpOne  :: FoldInOut io => Exp io WithoutFold
    ExpId   :: Id -> Exp inout WithoutFold
    ExpIf0  :: ( AtMostOneOccurrenceOfFold a
               , AtMostOneOccurrenceOfFold b
               , AtMostOneOccurrenceOfFold c
               , FoldInOut io ) =>
               Exp io a -> Exp io b -> Exp io c -> Exp io (a :+: b :+: c)
    ExpFold :: Exp OutFold WithoutFold -> Exp OutFold WithoutFold ->
               Id -> Id -> Exp InFold WithoutFold -> Exp OutFold WithFold
    ExpUOp  :: ( AtMostOneOccurrenceOfFold a
               , FoldInOut io ) =>
               UnaryOp -> Exp io a -> Exp io a
    ExpBOp  :: ( AtMostOneOccurrenceOfFold a
               , AtMostOneOccurrenceOfFold b
               , FoldInOut io ) =>
               BinaryOp -> Exp io a -> Exp io b -> Exp io (a :+: b)

instance (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Eq (Exp io fold) where
    a == b = show a == show b

instance (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Show (Exp io fold) where
    show ExpZero = "0"
    show ExpOne = "1"
    show (ExpId ident) = show ident
    show (ExpIf0 e0 e1 e2) =  "(if0 " ++ shows e0 " " ++ shows e1 " " ++ shows e2 ")"
    show (ExpFold e0 e1 id0 id1 e2) = "(fold " ++ shows e0 " " ++ shows e1 " (lambda (" ++ shows id0 " " ++ shows id1 ") " ++ shows e2 "))"
    show (ExpUOp op e0) = "(" ++ shows op " " ++ shows e0 ")"
    show (ExpBOp op e0 e1) = "(" ++ shows op " " ++ shows e0 " " ++ shows e1 ")"

-- | op1 ::=
data UnaryOp = UnaryOpNot -- ^ "not"
             | UnaryOpShl1 -- ^ "shl1"
             | UnaryOpShr1 -- ^ "shr1"
             | UnaryOpShr4 -- ^ "shr4"
             | UnaryOpShr16 -- ^ "shr16"
               deriving (Eq)

instance Show UnaryOp where
    show UnaryOpNot = "not"
    show UnaryOpShl1 = "shl1"
    show UnaryOpShr1 = "shr1"
    show UnaryOpShr4 = "shr4"
    show UnaryOpShr16 = "shr16"

-- | op2 ::=
data BinaryOp = BinaryOpAnd -- ^ "and"
              | BinaryOpOr -- ^ "or"
              | BinaryOpXor -- ^ "xor"
              | BinaryOpPlus -- ^ "plus"
                deriving (Eq)

instance Show BinaryOp where
    show BinaryOpAnd = "and"
    show BinaryOpOr = "or"
    show BinaryOpXor = "xor"
    show BinaryOpPlus = "plus"

-- | id  ::= [a-z]+
-- Int > 0
data Id = Id !Int
          deriving (Eq)

instance Show Id where
    show (Id 0) = "x"
    show (Id 1) = "y"
    show (Id 2) = "z"

sizeOfProgram :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Program fold -> Int
sizeOfProgram (Program _ e) = 1 + sizeOfExp e

sizeOfExp :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Exp io fold -> Int
sizeOfExp ExpZero = 1
sizeOfExp ExpOne = 1
sizeOfExp (ExpId _) = 1
sizeOfExp (ExpIf0 e0 e1 e2) = 1 + sizeOfExp e0 + sizeOfExp e1 + sizeOfExp e2
sizeOfExp (ExpFold e0 e1 _ _ e2) = 2 + sizeOfExp e0 + sizeOfExp e1 + sizeOfExp e2
sizeOfExp (ExpUOp _ e0) = 1 + sizeOfExp e0
sizeOfExp (ExpBOp _ e0 e1) = 1 + sizeOfExp e0 + sizeOfExp e1

opsOfProgram :: AtMostOneOccurrenceOfFold fold => Program fold -> [String]
opsOfProgram (Program _ e) = opsOfExp e

opsOfExp :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Exp io fold -> [String]
opsOfExp ExpZero = []
opsOfExp ExpOne = []
opsOfExp (ExpId _) = []
opsOfExp (ExpIf0 e0 e1 e2) = nub $ "if0" : opsOfExp e0 ++ opsOfExp e1 ++ opsOfExp e2
opsOfExp (ExpFold e0 e1 _ _ e2) = nub $ "fold" : opsOfExp e0 ++ opsOfExp e1 ++ opsOfExp e2
opsOfExp (ExpUOp op e0) = nub $ show op : opsOfExp e0
opsOfExp (ExpBOp op e0 e1) = nub $ show op : opsOfExp e0 ++ opsOfExp e1

uopsOfProgram :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Program fold -> [UnaryOp]
uopsOfProgram (Program _ e) = uopsOfExp e

uopsOfExp :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Exp io fold -> [UnaryOp]
uopsOfExp ExpZero = []
uopsOfExp ExpOne = []
uopsOfExp (ExpId _) = []
uopsOfExp (ExpIf0 e0 e1 e2) = nub $ uopsOfExp e0 ++ uopsOfExp e1 ++ uopsOfExp e2
uopsOfExp (ExpFold e0 e1 _ _ e2) = nub $ uopsOfExp e0 ++ uopsOfExp e1 ++ uopsOfExp e2
uopsOfExp (ExpUOp op e0) = nub $ op : uopsOfExp e0
uopsOfExp (ExpBOp _ e0 e1) = nub $ uopsOfExp e0 ++ uopsOfExp e1

bopsOfProgram :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Program fold -> [BinaryOp]
bopsOfProgram (Program _ e) = bopsOfExp e

bopsOfExp :: (AtMostOneOccurrenceOfFold fold, FoldInOut io) => Exp io fold -> [BinaryOp]
bopsOfExp ExpZero = []
bopsOfExp ExpOne = []
bopsOfExp (ExpId _) = []
bopsOfExp (ExpIf0 e0 e1 e2) = nub $ bopsOfExp e0 ++ bopsOfExp e1 ++ bopsOfExp e2
bopsOfExp (ExpFold e0 e1 _ _ e2) = nub $ bopsOfExp e0 ++ bopsOfExp e1 ++ bopsOfExp e2
bopsOfExp (ExpUOp _ e0) = nub $ bopsOfExp e0
bopsOfExp (ExpBOp op e0 e1) = nub $ op : bopsOfExp e0 ++ bopsOfExp e1
