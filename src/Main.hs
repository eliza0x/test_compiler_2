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

expr :: Expr
expr = EBind "main" [] (
    TLet [ EBind "fact" ["n"] $ TIf (TAInst Eq (TLabel "n") (TInt 0))
                                    (TInt 1)
                                    (TInt 5)
         ] $ TCall "fact" [TInt 5])

fromExpr :: Expr -> IO (Block [IR])
fromExpr (EBind name args term) =
    Block name args <$> runKnorm (Tag "_return" term)

runKnorm :: Tag Term -> IO [IR]
runKnorm term = do 
    a <- execWriterT . knorm $ term
    return $ reverse a

knorm :: Tag Term -> WriterT [IR] IO ()
knorm (Tag name originalTerm) = do
    let uuid   = "a"
    let uuid'  = "b"
    let uuids  = repeat "d"
    case originalTerm of
        TInt   int              -> tell [IInt name int]
        TAInst op term term'    -> tell [IAInst op name uuid uuid'] 
                                >> knorm (Tag uuid term)
                                >> knorm (Tag uuid' term')
        TIf    cond then' else' -> do
            tcond <- lift $ runKnorm (Tag "_return" cond)
            tthen <- lift $ runKnorm (Tag name then')
            telse <- lift $ runKnorm (Tag name else')
            tell [IIf tcond tthen telse]
        TCall  label args       -> do
            let uuids' = take (length args) uuids
            tell [ICall label uuids']
            mapM_ knorm $ zipWith Tag uuids' args
        TLabel label            -> tell [ILabel name label]
        TLet   exprs term       -> do
            blocks <- lift $ mapM fromExpr exprs
            irs    <- lift $ runKnorm (Tag "_return" term)
            tell [ILet blocks irs]

main :: IO ()
main = print =<< fromExpr expr

