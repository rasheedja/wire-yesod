{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Followers where


import           Import

-- | Return a list of users who are following the given user
getFollowersR :: Text -> Handler Value
getFollowersR username = do
    maybeUser <- runDB $ selectFirst [UserUsername ==. username] []
    case maybeUser of
        Just (Entity userId _) -> do
            users <- runDB $ selectList [FollowFollowingId ==. userId] []
            returnJson users
        Nothing -> returnJson $ object ["success" .= success, "message" .= message]
            where
                success = False :: Bool
                message = "The given username was not found" :: Text
