{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BFMonad where

import Control.Monad.State
import Control.Monad.Writer


type Prog = [Stmt]

data Stmt = One       | Len         | Pop        | Swp
          | Push Prog | Negate Prog | While Prog | Ignore Prog
  deriving (Eq, Show)


newtype BF a = BF { unBF :: StateT (Integer,Int) (Writer String) a } deriving
  (Functor, Applicative, Monad, MonadState (Integer,Int), MonadWriter String)

execBF :: BF a -> String
execBF bf = execWriter (evalStateT (unBF bf) (0,4))

emit :: String -> BF ()
emit instr = do
  indent <- snd <$> get
  tell (replicate indent ' ' ++ instr ++ "\n")

indented :: BF a -> BF ()
indented bf = chg 2 >> bf >> chg (-2)
  where chg z = modify (\(n,i) -> (n,i+z)) :: BF ()

newLabelNum :: BF String
newLabelNum = do
  num <- fst <$> get
  modify (\(n,i) -> (n+1,i))
  return $ show num
