{-# LANGUAGE LambdaCase,QuasiQuotes #-}

module Compiler (OS(..), linux, windows, osx, parseProg, compile) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Text.Parsec

import AsmTemplates
import BFMonad
import Quoter


data OS = OS {dict :: M.Map String String, gcc  :: String, exe  :: String}

linux   = OS (mkDict ""  4096 `mappend` err) "gcc" ""
  where err = M.fromList [("push_stderr", "push stderr")]
windows = OS (mkDict "_" 4096 `mappend` err) "gcc.exe" ".exe"
  where err = M.fromList [("push_stderr", ""), ("fprintf","printf")]  -- windows has no stderr(?)
osx     = OS (mkDict "_" 4096 `mappend` err) "gcc" ""
  where err = M.fromList [("push_stderr", "push _stderr")]

mkDict :: String -> Int -> M.Map String String
mkDict pre sz = M.fromList $
    [(l, pre ++ l) | l <- labels] ++ [("stack_size",show sz),("stack_size_bytes",show $ 4*sz)]

  where labels = [ "exit", "fprintf", "free", "main", "malloc", "printf", "realloc", "sscanf"]


parseProg :: String -> Either ParseError Prog
parseProg = parse (progP <* eof) "src" . dropWhileEnd (`notElem` brackets)
  where
    progP = many $ choice [paren cs n m | (cs,n,m) <- funcs]

    funcs = [ ("()",One,Push)
            , ("[]",Len,Negate)
            , ("{}",Pop,While)
            , ("<>",Swp,Ignore)
            ]

    paren (a:b:_) nilad monad = between (char' a) (char' b) progP >>= \case
      [] -> return nilad
      ps -> return (monad ps)
    paren _ _ _ = error "won't happen"

    char' c = dropCs *> char c <* dropCs

    dropCs = void . many $ noneOf brackets

    brackets = "()[]{}<>"


--------------------- | Memory layout | ----------------------
--
-- EAX: accumulator
-- EBX: base pointer of active stack
-- ECX: offset of active stack
-- EDX: maximum offset possible for active stack
--
-- EBP: points to data of other stack
--       8(%EBP) -> base pointer
--       4(%EBP) -> offset
--        (%EBP) -> maximum offset
--
--------------------------------------------------------------
compile :: Prog -> OS -> BF ()
compile prog os = header (dict os) >> mapM_ go prog >> footer (dict os)

  where go x = emit ("# " ++ comment x) >> go' x

        comment One = "() : 1"
        comment Len = "[] : height of stack"
        comment Pop = "{} : pop from active stack"
        comment Swp = "<> : toggle stack"
        comment (Push   _) = "(X) : push X"
        comment (Negate _) = "[X] : negate X"
        comment (While  _) = "{X} : while top != 0; do X"
        comment (Ignore _) = "<X> : do X but return 0"

        go' One = [asm| add $1,%eax   |]
        go' Len = [asm| add %ecx,%eax |]
        go' Pop = [asm| call pop_add  |]

        go' Swp = [asm|
          xchg 8(%ebp),%ebx
          xchg 4(%ebp),%ecx
          xchg  (%ebp),%edx
        |]

        go' (Push p) = [asm|
          push %eax
          xor %eax,%eax
          {sub}
          call push_acc
          pop %esi
          add %esi,%eax
        |] $ mapM_ go p

        go' (Negate p) = [asm|
          push %eax
          xor %eax,%eax
          {sub}
          neg %eax
          pop %esi
          add %esi,%eax
        |] $ mapM_ go p

        go' (Ignore p) = [asm|
          push %eax
          xor %eax,%eax
          {sub}
          pop %eax
        |] $ mapM_ go p

        go' (While p) = do
          num <- newLabelNum
          emit $ "jmp loop_cond_" ++ num
          emit $ "loop_" ++ num ++ ":"
          indented $ mapM_ go p
          emit $ "loop_cond_" ++ num ++ ":"
          indented $ do
            emit "cmp $0,(%ebx,%ecx,4)"
            emit $ "jne loop_" ++ num
