{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Follow where


import Import
import Data.Aeson

getFollowR :: Handler Value
getFollowR = do
    maybeFollowingName <- lookupGetParam "username"
    case maybeFollowingName of
        Just followingName -> do
            maybeFollowingUser <- runDB $ selectFirst [UserUsername ==. followingName] []
            case maybeFollowingUser of
                Just (Entity followingUserId _) -> do
                    maybeUser <- maybeAuth
                    case maybeUser of
                        Just (Entity userId _) -> do
                            maybeFollowing <- runDB $ selectFirst [FollowFollowerId ==. userId, FollowFollowingId ==. followingUserId] []
                            case maybeFollowing of
                                Just _ -> do
                                    runDB $ deleteWhere [FollowFollowerId ==. userId, FollowFollowingId ==. followingUserId]
                                    returnJson $ object ["success" .= success, "message" .= message]
                                    where
                                        success = True :: Bool
                                        message = "You have successfully unfollowed " ++ followingName :: Text
                                Nothing -> do
                                    runDB $ insert $ Follow userId followingUserId
                                    returnJson $ object ["success" .= success, "message" .= message]
                                    where
                                        success = True :: Bool
                                        message = "You have successfully followed " ++ followingName :: Text
                        Nothing -> do
                            returnJson $ object ["success" .= success, "message" .= message]
                            where
                                success = False :: Bool
                                message = "You must be logged in to follow a user" :: Text
                Nothing -> do
                    returnJson $ object ["success" .= success, "message" .= message]
                    where
                        success = False :: Bool
                        message = "The user you tried to follow was not found" :: Text
        Nothing ->
            returnJson $ object ["success" .= success, "message" .= message]
            where
                success = False :: Bool
                message = "The ID of a user to follow must be given" :: Text
