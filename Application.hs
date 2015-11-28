{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Application
    ( getApplication
    ) where

import           ClassyPrelude                        (handleAny)
import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Monad                        (forM_, unless, void, forever)
import           Data.IORef                           (newIORef, writeIORef)
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Filesystem                           (isDirectory)
import qualified Filesystem.Path.CurrentOS            as F
import           Import
import           Network.Wai.Application.Static       (defaultFileServerSettings)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           System.Environment                   (getEnvironment)
import           System.Exit                          (ExitCode (ExitSuccess),
                                                       exitWith)
import           System.Process                       (rawSystem, runProcess,
                                                       waitForProcess)
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Static                         (Static (Static))

-- Import all relevant handler modules here.
import           Handler.Blog
import           Handler.Page
import           Handler.Root
import           Handler.Wiki


-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "LambdaWeb" resourcesLambdaWeb

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> IO Application
getApplication conf = do
    env <- getEnvironment
    forM_ branches $ \(dir, branch) -> do
        exists <- isDirectory $ F.decodeString dir
        unless exists $ do
            putStrLn $ "Cloning " ++ dir
            ec <- rawSystem "git"
                [ "clone"
                , "-b"
                , branch
                , fromMaybe "https://github.com/mlitchard/lambdaweb.com-content.git"
                  $ lookup "CONTENT_REPO" env
                , dir
                ]
            unless (ec == ExitSuccess) $ do
                putStrLn "git clone failed, exiting"
                exitWith ec

    s <- staticSite
    let assets = Static $ defaultFileServerSettings "content/static"

    eblog <- loadBlog
    blog <-
        case eblog of
            Left e -> do
                print e
                return $ error $ "Invalid posts.yaml: " ++ show e
            Right b -> return b
    iblog <- newIORef blog
    iauthors <- loadAuthors >>= newIORef
    let foundation = LambdaWeb
            { settings = conf
            , getStatic = s
            , getAssets = assets
            , ywBlog = iblog
            , ywAuthors = iauthors
            }
    app <- toWaiAppPlain foundation
    logWare <- mkLogWare

    -- Reload git every 5 minutes, due to lack of propagation of webhooks in a
    -- horizontally scaled situation
    void $ forkIO $ forever $ do
        threadDelay 300000000
        handleAny print $ pullGitBranches foundation

    return $ gzip def
           $ autohead
           $ logWare
             app
  where
    mkLogWare = mkRequestLogger def
        { outputFormat = Apache FromHeader
        }


dir12, dir11, dir14 :: FilePath
dir12 = "content-1.2"
dir11 = "content-1.1"
dir14 = "content"

branches :: [(FilePath, String)]
branches =
    [ (dir12, "version1.2")
    , (dir11, "version1.1")
    , (dir14, "master")
    ]

postReloadR :: Handler ()
postReloadR = do
    yw <- getYesod
    void $ liftIO $ forkIO $ pullGitBranches yw

pullGitBranches :: LambdaWeb -> IO ()
pullGitBranches yw = do
    forM_ branches $ \(dir, branch) -> do
        let run x y = void $ runProcess x y (Just dir) Nothing Nothing Nothing Nothing >>= waitForProcess
        run "git" ["fetch"]
        run "git" ["checkout", "origin/" ++ branch]
    eblog <- loadBlog
    case eblog of
        Left e -> print e
        Right blog -> writeIORef (ywBlog yw) blog
    loadAuthors >>= writeIORef (ywAuthors yw)
