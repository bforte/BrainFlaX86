{-# LANGUAGE DeriveLift,TemplateHaskell #-}

module Quoter (asm, str) where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parsec

import BFMonad


data Str a = Lit a | Var String deriving (Show,Lift)


asm = QuasiQuoter
  { quoteExp  = quoteAsm
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

quoteAsm str = do
  let (a,_b) = break ("{sub}"==) $ filter (not.null)
                 [dropWhileEnd isSpace $ dropWhile isSpace l | l <- lines str]
  case _b of
    []  -> [e| mapM_ emit a |]
    _:b -> [e| \bf-> mapM_ emit a >> indented bf >> mapM_ emit b |]


str = QuasiQuoter
  { quoteExp  = quoteStr
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

quoteStr str = case parse (strP <* eof) "th" str of
  Left e  -> error (show e)
  Right s | empty s   -> [e| emit str |]
          | otherwise -> [e| \d-> emit $ s `replace` d |]

  where strP :: Parsec String () [Str Char]
        strP = many (varP <|> litP)

        varP = Var <$> between (char '{') (char '}') (many $ noneOf "}")
        litP = Lit <$> anyChar

        empty s = null [() | Var _ <- s]


replace dstr dict = dstr >>= go
  where go (Lit c) = [c]
        go (Var v) =
          fromMaybe (error $ "couldn't find '" ++ v ++ "'") $ M.lookup v dict
