module Handler.Wiki
    ( getWikiHomeR
    ) where

import Import
import Network.HTTP.Types (status301)
import qualified Data.Text as T

getWikiHomeR :: Handler Html
getWikiHomeR = redirect ("https://github.com/mlitchard" :: T.Text)
