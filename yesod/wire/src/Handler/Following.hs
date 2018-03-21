{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Following where


import Import

getFollowingR :: Text -> Handler Value
getFollowingR username = do
    maybeUser <- runDB $ selectFirst [UserUsername ==. username] []
    case maybeUser of
        Just (Entity userId _) -> do
            users <- runDB $ selectList [FollowFollowerId ==. userId] []
            returnJson users
        Nothing -> do
            returnJson $ object ["success" .= success, "message" .= message]
            where
                success = False :: Bool
                message = "The given username was not found" :: Text
