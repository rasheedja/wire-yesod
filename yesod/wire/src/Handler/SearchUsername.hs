{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SearchUsername where

import           Import

-- | Load the search results that match the given query and then render the
-- template for user search results
getSearchUsernameR :: Text -> Handler Html
getSearchUsernameR query = do
    results <- runDB $ selectList [Filter UserUsername (Left $ Import.concat ["%", query, "%"]) (BackendSpecificFilter "ILIKE")] []

    defaultLayout $ do
        setTitle "User Search Results"
        $(widgetFile "search-results-user")
