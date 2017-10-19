module Main where

import Data.List
import Control.Monad.Writer.Strict

-- オペレータ
data Op = Eq
        | Mul
        | Sub
        deriving (Show, Eq)

-- 抽象構文木
data Expr = EBind String [String] Term
          deriving (Eq, Show)
          
data Term = TIf Term Term Term
          | TLet   [Expr] Term
          | TAInst Op Term Term
          | TInt   Int
          | TLabel String
          | TCall  String [Term]
          deriving (Eq, Show)

-- ラベル付き塊
data Block a = Block String [String] a
          deriving (Eq, Show)

-- IR
data IR = ILet   [Block [IR]] [IR]
        | IIf    [IR] [IR] [IR]
        | ICall  String [String]
        | IAInst Op String String String
        | IBind  IR Int
        | ILabel String String
        | IInt   String Int
        deriving (Eq, Show)

-- Assembry
data Asm = LBoe      String String
         | LJump     String
         | Label     String
         | LDefLabel String
         | LMul      String String String
         | LSub      String String String
         | LEq       String String String
         | LBind     String Int
         deriving (Eq, Show)

data Tag a = Tag String a

main :: IO ()
main = print =<< fromExpr expr

