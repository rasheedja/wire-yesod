{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Text.Julius

-- Check if the given username exists in the database and if it does, load the
-- profile page for the specified user. If the username does not exist, load
-- the search page.
getProfileR :: Text -> Handler Html
getProfileR username = do
    let jsUsername = rawJS username
    maybeUser <- runDB $ getBy $ UniqueUser username
    case maybeUser of
        Just (Entity userId user) -> do
            defaultLayout $ do
                setTitle . toHtml $ userUsername user
                $(widgetFile "profile")
        Nothing -> do
            defaultLayout $ do
                -- TODO: Redirect to user search page
                setSession "msgrendered" "true"
                setMessage $ renderErrorMessage "Username not found"
                redirect HomeR
