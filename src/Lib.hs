{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Attoparsec.Text
import qualified Data.Text.Lazy as L

data ModuleName
  = ModuleName String
  deriving(Show)

data Identifier
  = Identifier String
  deriving(Show)

data Arguments
  = Arguments String
  deriving(Show)

data Body
  = Body [Statement]
  deriving(Show)

data Statement
  = FunctionCall Identifier Arguments
  deriving(Show)

data FunctionDefinition
  = FunctionDefinition Identifier Arguments Body
  deriving(Show)

data Golo
  = Golo ModuleName FunctionDefinition
  deriving(Show)

parseGoloCode :: Parser Golo
parseGoloCode = do
  mod <- parseModuleDefinition
  fn_def <- parseFunctionDefinition
  return $ Golo mod fn_def

parseFunctionCall :: Parser Statement
parseFunctionCall = do
  function_name <- manyTill anyChar (char '(')
  args <- manyTill anyChar (char ')')
  endOfLine

  return $ FunctionCall
             (Identifier function_name)
             (Arguments args)

parseStatement :: Parser Statement
parseStatement = do
  try parseFunctionCall

parseBody :: Parser Body
parseBody = do
  b <- manyTill anyChar (char '}')
  let exprs = parseOnly (many1 parseStatement) $ L.toStrict $ L.pack b
  case exprs of
    Left reason -> fail $ "parseBody fail: " ++ reason
    Right exs ->   return $ Body exs

parseFunctionDefinition :: Parser FunctionDefinition
parseFunctionDefinition = do
  string "function"
  skipMany space
  function_name <- manyTill anyChar (char ' ')
  manyTill space (char '=')
  manyTill space (char '|')
  args <- manyTill anyChar (char '|')
  manyTill space (char '{')
  skipMany space
  body <- parseBody

  return $ FunctionDefinition
    (Identifier function_name)
    (Arguments args)
    body

parseModuleDefinition :: Parser ModuleName
parseModuleDefinition = do
  string "module"
  skipMany space
  module_name <- manyTill anyChar endOfLine
  return $ ModuleName module_name

someFunc :: IO ()
someFunc = print $ parseOnly parseGoloCode "module golo.demo\nfunction main = |args| {\nprintln(\"Hello world\")\nprintln(\"Bonjour le monde!\")\n}\n\n"
