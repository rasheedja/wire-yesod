{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SearchMessage where

import           Helper.Message
import           Import

-- | Load the search results that match the given query and then render the
-- template for message search results
getSearchMessageR :: Text -> Handler Html
getSearchMessageR query = do
    results <- runDB $ selectList [Filter MessageMessage (Left $ Import.concat ["%", query, "%"]) (BackendSpecificFilter "ILIKE")] []
    let posterIds = Import.map (\(Entity _ (Message _ posterId _)) -> posterId) results
    posters <- runDB $ selectList [UserId <-. posterIds] []

    let formattedResults = Import.map (formatMessageEntity posters) results

    defaultLayout $ do
        setTitle "Wire Search Results"
        $(widgetFile "search-results-message")
