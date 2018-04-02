{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetAllExcludingUsername where


import           Import

-- | Return registered users in JSON format excluding the given user
-- and the currently logged in user. Does not return the password or
-- email address of the users
getUserGetAllExcludingUsernameR :: Text -> Handler Value
getUserGetAllExcludingUsernameR username = do
    maybeUser <- maybeAuth
    case maybeUser of
        Just (Entity _ user) -> do
            users <- runDB $ selectList [UserUsername !=. userUsername user, UserUsername !=. username] [LimitTo 5]
            let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
            returnJson cleanUsers
        Nothing -> do
            users <- runDB $ selectList [UserUsername !=. username] [LimitTo 5]
            let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
            returnJson cleanUsers
