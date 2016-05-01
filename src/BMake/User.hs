{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BMake.User
       ( parseWithAlex
       , parseMakefile
       , stateBase
       , parse
       , Error(..)
       ) where

import           Control.DeepSeq            (NFData)
import           Control.DeepSeq            (force)
import           Control.Exception          (evaluate)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.DList                 (DList)
import qualified Data.DList                 as DList
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           GHC.Generics               (Generic)
import           Lib.FilePath               (FilePath, (</>))
import qualified Lib.FilePath               as FilePath
import           Prelude.Compat             hiding (FilePath)

import           BMake.Base
import           BMake.Interpreter          (interpret)
import           BMake.Parser               (happyParser)
import qualified Lib.Makefile.Types         as MT

data Error = Error !Int !Int !String
   deriving (Show, Generic)

instance NFData Error

stateBase :: Int
stateBase = 0

parseWithAlex :: Int -> BL.ByteString -> Either String (DList Token)
parseWithAlex startState bs = root
    where root = runAlex bs (alexSetStartCode startState >> loop DList.empty)
          loop s' = do
              lexer $ \token@(Token _ cls) -> do
                  case cls of
                      TokenEOF -> return s'
                      _ -> loop $ s' `DList.snoc` token


parseMakefile :: BL.ByteString -> Either Error Makefile
parseMakefile s =
  case runAlex s $ happyParser of
    Right x -> Right x
    Left ('l':'e':'x':xs) ->
      Left (Error 0 0 xs) -- TODO
    Left ('s':'h':'o':'w':'-':'e':'r':'r':'o':'r':':':' ':xs) ->
      let (line, column, e) = (read xs :: (Int, Int, String))
       in Left (Error line column (e :: String))
    Left xs -> Left (Error 0 0 xs)

type Cache k v = IORef (Map k v)

type ParseCache = Cache FilePath Makefile

data Dirs = Dirs
    { dirsRoot        :: FilePath
    , dirsCurMakefile :: FilePath
    }

handleIncludePath :: ParseCache -> Dirs -> FilePath -> IO [Statement]
handleIncludePath cache dirs = fmap unit . newParse cache (dirsRoot dirs)

handleIncludeStr :: ParseCache -> Dirs -> FilePath -> IO [Statement]
handleIncludeStr cache dirs path =
    case "/" `B8.isPrefixOf` path of
        True ->  handleIncludePath cache dirs $ dirsRoot dirs </> (B8.drop 1 path)
        False -> handleIncludePath cache dirs $ dirsCurMakefile dirs </> path

handleInclude :: ParseCache -> Dirs -> Statement -> IO [Statement]
handleInclude cache dirs (Include path) =
    case reads pathStr of
    [(quotedPath, "")] -> handleIncludeStr cache dirs quotedPath
    _ -> handleIncludeStr cache dirs $ B8.pack pathStr
    where
        pathStr = BL8.unpack path
handleInclude cache dirs other =
    (: []) <$> substmts (handleIncludes cache dirs) other

handleIncludes :: ParseCache -> Dirs -> [Statement] -> IO [Statement]
handleIncludes cache dirs = fmap concat . mapM (handleInclude cache dirs)

parseSingle :: BL8.ByteString -> IO (Either Error Makefile)
parseSingle = evaluate . force . parseMakefile

memoIO :: Ord k => Cache k v -> (k -> IO v) -> k -> IO v
memoIO cache action k =
    do
        m <- readIORef cache
        case Map.lookup k m of
            Just v -> return v
            Nothing ->
                do
                    v <- action k
                    modifyIORef cache $ Map.insert k v
                    return v

newParse :: ParseCache -> FilePath -> FilePath -> IO Makefile
newParse cache rootDir =
    memoIO cache $ \makefile -> do
        content <- BL.readFile $ B8.unpack makefile
        let dirs = Dirs rootDir (FilePath.takeDirectory makefile)
        res <- parseSingle content
        case res of
            Left (Error line col str) -> do
                fail $ B8.unpack makefile ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ str
            Right ast -> do
                ast' <- Makefile <$> handleIncludes cache dirs (unit ast)
                return ast'

parse :: FilePath -> MT.Vars -> IO MT.Makefile
parse makefilePath vars = do
   cache <- newIORef Map.empty
   ast <- newParse cache (FilePath.takeDirectory makefilePath) makefilePath
   interpret ast vars
