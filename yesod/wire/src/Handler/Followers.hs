{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Followers where


import Import

getFollowersR :: Text -> Handler Value
getFollowersR username = do
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