{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetAll where


import           Import

-- Return registered users in JSON format excluded the currently logged in user
getUserGetAllR :: Handler Value
getUserGetAllR = do
    maybeUser <- maybeAuth
    case maybeUser of
        Just (Entity _ user) -> do
            users <- runDB $ selectList [UserUsername !=. userUsername user] [LimitTo 5]
            let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
            returnJson cleanUsers
        Nothing -> do
            users <- runDB $ selectList [UserUsername !=. ""] [LimitTo 5]
            let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
            returnJson cleanUsers
