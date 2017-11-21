{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main(main) where

import           System.Exit        (exitSuccess)
import           System.Environment (getArgs)
import           Control.Monad      (forM_)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.JuPyTer(IPyNb(..), CellsElt(..), parse)

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f
                                                            ++ ":\n" ++ extract p))
  exitSuccess

extractCell :: CellsElt -> String
extractCell c | cellsEltCellType c == "code"     = cellContents c
extractCell c | cellsEltCellType c == "markdown" = addComments $ cellContents c
extractCell _                                    = ""

comment :: String
comment  = "#"

addComments :: String -> String
addComments  = unlines . map (comment++) . lines

cellContents :: CellsElt -> String
cellContents = concatMap (alt Text.unpack $ unlines . map (maybe "" show))
             . cellsEltSource

extract :: IPyNb -> String
extract = unlines
        . map extractCell
        . iPyNbCells

