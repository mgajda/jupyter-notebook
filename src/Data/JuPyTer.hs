{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Data.JuPyTer where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data CodemirrorMode = CodemirrorMode { 
    codemirrorModeName :: Text,
    codemirrorModeVersion :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON CodemirrorMode where
  parseJSON (Object v) = CodemirrorMode <$> v .:   "name" <*> v .:   "version"
  parseJSON _          = mzero


instance ToJSON CodemirrorMode where
  toJSON     (CodemirrorMode {..}) = object ["name" .= codemirrorModeName, "version" .= codemirrorModeVersion]
  toEncoding (CodemirrorMode {..}) = pairs  ("name" .= codemirrorModeName<>"version" .= codemirrorModeVersion)


data LanguageInfo = LanguageInfo { 
    languageInfoNbconvertExporter :: Text,
    languageInfoFileExtension :: Text,
    languageInfoPygmentsLexer :: Text,
    languageInfoMimetype :: Text,
    languageInfoName :: Text,
    languageInfoVersion :: Text,
    languageInfoCodemirrorMode :: CodemirrorMode
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON LanguageInfo where
  parseJSON (Object v) = LanguageInfo <$> v .:   "nbconvert_exporter" <*> v .:   "file_extension" <*> v .:   "pygments_lexer" <*> v .:   "mimetype" <*> v .:   "name" <*> v .:   "version" <*> v .:   "codemirror_mode"
  parseJSON _          = mzero


instance ToJSON LanguageInfo where
  toJSON     (LanguageInfo {..}) = object ["nbconvert_exporter" .= languageInfoNbconvertExporter, "file_extension" .= languageInfoFileExtension, "pygments_lexer" .= languageInfoPygmentsLexer, "mimetype" .= languageInfoMimetype, "name" .= languageInfoName, "version" .= languageInfoVersion, "codemirror_mode" .= languageInfoCodemirrorMode]
  toEncoding (LanguageInfo {..}) = pairs  ("nbconvert_exporter" .= languageInfoNbconvertExporter<>"file_extension" .= languageInfoFileExtension<>"pygments_lexer" .= languageInfoPygmentsLexer<>"mimetype" .= languageInfoMimetype<>"name" .= languageInfoName<>"version" .= languageInfoVersion<>"codemirror_mode" .= languageInfoCodemirrorMode)


data Kernelspec = Kernelspec { 
    kernelspecDisplayName :: Text,
    kernelspecName :: Text,
    kernelspecLanguage :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Kernelspec where
  parseJSON (Object v) = Kernelspec <$> v .:   "display_name" <*> v .:   "name" <*> v .:   "language"
  parseJSON _          = mzero


instance ToJSON Kernelspec where
  toJSON     (Kernelspec {..}) = object ["display_name" .= kernelspecDisplayName, "name" .= kernelspecName, "language" .= kernelspecLanguage]
  toEncoding (Kernelspec {..}) = pairs  ("display_name" .= kernelspecDisplayName<>"name" .= kernelspecName<>"language" .= kernelspecLanguage)


data Slideshow = Slideshow { 
    slideshowSlideType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Slideshow where
  parseJSON (Object v) = Slideshow <$> v .:   "slide_type"
  parseJSON _          = mzero


instance ToJSON Slideshow where
  toJSON     (Slideshow {..}) = object ["slide_type" .= slideshowSlideType]
  toEncoding (Slideshow {..}) = pairs  ("slide_type" .= slideshowSlideType)


data Metadata = Metadata { 
    metadataScrolled :: (Maybe (Bool:|:[(Maybe Value)])),
    metadataCelltoolbar :: (Maybe (Text:|:[(Maybe Value)])),
    metadataLanguageInfo :: (Maybe (LanguageInfo:|:[(Maybe Value)])),
    metadataKernelspec :: (Maybe (Kernelspec:|:[(Maybe Value)])),
    metadataEditable :: (Maybe (Bool:|:[(Maybe Value)])),
    metadataDeletable :: (Maybe (Bool:|:[(Maybe Value)])),
    metadataSlideshow :: (Maybe (Slideshow:|:[(Maybe Value)])),
    metadataCollapsed :: (Maybe (Bool:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> v .:?? "scrolled" <*> v .:?? "celltoolbar" <*> v .:?? "language_info" <*> v .:?? "kernelspec" <*> v .:?? "editable" <*> v .:?? "deletable" <*> v .:?? "slideshow" <*> v .:?? "collapsed"
  parseJSON _          = mzero


instance ToJSON Metadata where
  toJSON     (Metadata {..}) = object ["scrolled" .= metadataScrolled, "celltoolbar" .= metadataCelltoolbar, "language_info" .= metadataLanguageInfo, "kernelspec" .= metadataKernelspec, "editable" .= metadataEditable, "deletable" .= metadataDeletable, "slideshow" .= metadataSlideshow, "collapsed" .= metadataCollapsed]
  toEncoding (Metadata {..}) = pairs  ("scrolled" .= metadataScrolled<>"celltoolbar" .= metadataCelltoolbar<>"language_info" .= metadataLanguageInfo<>"kernelspec" .= metadataKernelspec<>"editable" .= metadataEditable<>"deletable" .= metadataDeletable<>"slideshow" .= metadataSlideshow<>"collapsed" .= metadataCollapsed)

data CellsElt = CellsElt { 
    cellsEltExecutionCount :: (Maybe (Double:|:[(Maybe Value)])),
    cellsEltOutputs :: (Maybe ([[(Maybe Value)]])),
    cellsEltMetadata :: Metadata,
    cellsEltSource :: [Text:|:[(Maybe Value)]],
    cellsEltCellType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON CellsElt where
  parseJSON (Object v) = CellsElt <$> v .:?? "execution_count" <*> v .:?? "outputs" <*> v .:   "metadata" <*> v .:   "source" <*> v .:   "cell_type"
  parseJSON _          = mzero


instance ToJSON CellsElt where
  toJSON     (CellsElt {..}) = object ["execution_count" .= cellsEltExecutionCount, "outputs" .= cellsEltOutputs, "metadata" .= cellsEltMetadata, "source" .= cellsEltSource, "cell_type" .= cellsEltCellType]
  toEncoding (CellsElt {..}) = pairs  ("execution_count" .= cellsEltExecutionCount<>"outputs" .= cellsEltOutputs<>"metadata" .= cellsEltMetadata<>"source" .= cellsEltSource<>"cell_type" .= cellsEltCellType)

data IPyNb = IPyNb { 
    iPyNbNbformatMinor :: Double,
    iPyNbNbformat :: Double,
    iPyNbCells :: [CellsElt],
    iPyNbMetadata :: Metadata
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON IPyNb where
  parseJSON (Object v) = IPyNb <$> v .:   "nbformat_minor" <*> v .:   "nbformat" <*> v .:   "cells" <*> v .:   "metadata"
  parseJSON _          = mzero


instance ToJSON IPyNb where
  toJSON     (IPyNb {..}) = object ["nbformat_minor" .= iPyNbNbformatMinor, "nbformat" .= iPyNbNbformat, "cells" .= iPyNbCells, "metadata" .= iPyNbMetadata]
  toEncoding (IPyNb {..}) = pairs  ("nbformat_minor" .= iPyNbNbformatMinor<>"nbformat" .= iPyNbNbformat<>"cells" .= iPyNbCells<>"metadata" .= iPyNbMetadata)




parse :: FilePath -> IO IPyNb
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: IPyNb)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
