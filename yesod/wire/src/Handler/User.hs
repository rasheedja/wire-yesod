{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.User where


import Import

-- Return registered users in JSON format
-- Users excluded from this get request
-- are the currently logged in user and the
-- the username specified in the request (if any)
getUserR :: Handler Value
getUserR = do
        maybeUsername <- lookupGetParam "username"
        maybeUser <- maybeAuth
        case maybeUsername of
            Just username -> do
                case maybeUser of
                    Just (Entity _ user) -> do
                        users <- runDB $ selectList [UserUsername !=. username, UserUsername !=. userUsername user] [LimitTo 5]
                        returnJson users
                    Nothing -> do
                        users <- runDB $ selectList [UserUsername !=. username] [LimitTo 5]
                        returnJson users
            Nothing -> do
                case maybeUser of
                    Just (Entity _ user) -> do
                        users <- runDB $ selectList [UserUsername !=. userUsername user] [LimitTo 5]
                        returnJson users
                    Nothing -> do
                        users <- runDB $ selectList [UserUsername !=. ""] [LimitTo 5]
                        returnJson users
