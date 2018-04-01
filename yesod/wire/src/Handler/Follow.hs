{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Follow where


import           Import

getFollowR :: Text -> Handler Value
getFollowR username = do
    maybeUser <- maybeAuth
    case maybeUser of
        Just (Entity userId _) -> do
            maybeFollowingUser <- runDB $ selectFirst [UserUsername ==. username] []
            case maybeFollowingUser of
                Just (Entity followingUserId _) -> do
                    maybeFollowing <- runDB $ selectFirst [FollowFollowerId ==. userId, FollowFollowingId ==. followingUserId] []
                    case maybeFollowing of
                        Just _ -> do
                            _ <- runDB $ deleteWhere [FollowFollowerId ==. userId, FollowFollowingId ==. followingUserId]
                            returnJson $ object ["success" .= success, "message" .= message]
                            where
                                success = True :: Bool
                                message = "You have successfully unfollowed " ++ username :: Text
                        Nothing -> do
                            _ <- runDB $ insert $ Follow userId followingUserId
                            returnJson $ object ["success" .= success, "message" .= message]
                            where
                                success = True :: Bool
                                message = "You have successfully followed " ++ username :: Text
                Nothing -> returnJson $ object ["success" .= success, "message" .= message]
                    where
                        success = False :: Bool
                        message = "The user you tried to follow was not found" :: Text
        Nothing -> returnJson $ object ["success" .= success, "message" .= message]
            where
                success = False :: Bool
                message = "You must be logged in to follow a user" :: Text
