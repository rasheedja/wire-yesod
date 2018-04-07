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
            messages <- runDB $ selectList [MessageUserId ==. userId] [Desc MessageCreated]
            returnJson messages
        Nothing -> returnJson $ object ["success" .= success, "message" .= message]
            where
                message = "The given username was not found" :: Text
                success = False :: Bool
