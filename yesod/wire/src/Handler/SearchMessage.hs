{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SearchMessage where

import           Import

getSearchMessageR :: Text -> Handler Html
getSearchMessageR query = do
    results <- runDB $ selectList [Filter MessageMessage (Left $ Import.concat ["%", query, "%"]) (BackendSpecificFilter "ILIKE")] []
    let posterIds = Import.map (\(Entity _ (Message _ posterId _)) -> posterId) results
    posters <- runDB $ selectList [UserId <-. posterIds] []

    defaultLayout $ do
        setTitle "Wire Search Results"
        $(widgetFile "search-results-message")
