{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Followers where


import Import
import Data.Aeson

getFollowersR :: Handler Value
getFollowersR = do
    maybeUsername <- lookupGetParam "username"
    case maybeUsername of
        Just username -> do
            maybeUser <- runDB $ selectFirst [UserUsername ==. username] []
            case maybeUser of
                Just (Entity userId _) -> do
                    users <- runDB $ selectList [FollowFollowingId ==. userId] []
                    returnJson users
                Nothing -> do
                    returnJson $ object ["success" .= success, "message" .= message]
                    where
                        success = False :: Bool
                        message = "The given username was not found" :: Text
        Nothing -> do
            returnJson $ object ["success" .= success, "message" .= message]
            where
                success = False :: Bool
                message = "Please provide a username" :: Text
