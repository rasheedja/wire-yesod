{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Profile where

import           Import

-- Check if the given username exists in the database and if it does, load the
-- profile page for the specified user. If the username does not exist, load
-- the search page.
getProfileR :: Text -> Handler Html
getProfileR username = do
    maybeUser <- runDB $ getBy $ UniqueUser username
    case maybeUser of
        Just (Entity _ user) -> defaultLayout $ do
            setTitle . toHtml $ userUsername user
            $(widgetFile "profile")
        Nothing -> defaultLayout $ do
            setSession "msgrendered" "true"
            setMessage $ renderErrorMessage "Username not found, tried a search..."
            redirect $ SearchUsernameR username
