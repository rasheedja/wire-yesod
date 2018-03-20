{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.User where


import Import

-- Return registered users in JSON format
-- Users excluded from this get request
-- are the currently logged in user, the
-- the username specified in the request (if any),
-- and anyone the currently logged in user is following
getUserR :: Handler Value
getUserR = do
    maybeUsername <- lookupGetParam "username"
    maybeUser <- maybeAuth
    case maybeUsername of
        Just username -> do
            case maybeUser of
                Just (Entity userId user) -> do
                    --FIXME: Figure out how to extract the followedIds from the list of Follow elements below
                    --FIXME: Use the extracted listed in the select below (UserId /<- followingIds)
                    followedUsers <- runDB $ selectList [FollowFollowerId ==. userId] []
                    users <- runDB $ selectList [UserUsername !=. username, UserUsername !=. userUsername user] [LimitTo 5]
                    returnJson users
                Nothing -> do
                    users <- runDB $ selectList [UserUsername !=. username] [LimitTo 5]
                    returnJson users
        Nothing -> do
            case maybeUser of
                Just (Entity userId user) -> do
                    --FIXME: Figure out how to extract the followedIds from the list of Follow elements below
                    --FIXME: Use the extracted listed in the select below (UserId /<- followingIds)
                    followedUsers <- runDB $ selectList [FollowFollowerId ==. userId] []
                    users <- runDB $ selectList [UserUsername !=. userUsername user] [LimitTo 5]
                    returnJson users
                Nothing -> do
                    users <- runDB $ selectList [UserUsername !=. ""] [LimitTo 5]
                    returnJson users

-- This request is used internally to get a list of
-- users from IDs provided via a request
postUserR :: Handler Value
postUserR = do
    returnJson $ object ["success" .= success, "message" .= message]
    where
        success = False :: Bool
        message = "Placeholder message" :: Text
