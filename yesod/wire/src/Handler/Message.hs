{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Message where


import Import
import Data.Aeson

-- Get all the messages posted by the given user
getMessageR :: Handler Value
getMessageR = do
    maybeUsername <- lookupGetParam "username"
    case maybeUsername of
        Just username -> do
            maybeUser <- runDB $ getBy $ UniqueUser username
            case maybeUser of
                Just (Entity userId user) -> do
                    messages <- runDB $ selectList [MessageUserId ==. userId] []
                    returnJson messages
                Nothing -> do
                    returnJson $ object ["message" .= message]
                    where
                        message = "The given user was not found" :: Text
        Nothing -> do
            returnJson $ object ["message" .= message]
            where
                message = "A username must be provided" :: Text

