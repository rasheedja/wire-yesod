{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Message where


import           Data.Aeson
import           Import


-- Get all the messages posted by the given user
getMessageR :: Text -> Handler Value
getMessageR username = do
    maybeUser <- runDB $ getBy $ UniqueUser username
    case maybeUser of
        Just (Entity userId _) -> do
            messages <- runDB $ selectList [MessageUserId ==. userId] []
            returnJson messages
        Nothing -> returnJson $ object ["message" .= message]
            where
                message = "The given user was not found" :: Text
