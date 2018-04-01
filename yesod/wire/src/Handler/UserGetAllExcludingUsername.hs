{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetAllExcludingUsername where


import           Import

-- Return registered users in JSON format excluding the given user
-- and the currently logged in user
getUserGetAllExcludingUsernameR :: Text -> Handler Value
getUserGetAllExcludingUsernameR username = do
    maybeUser <- maybeAuth
    case maybeUser of
        Just (Entity _ user) -> do
            users <- runDB $ selectList [UserUsername !=. userUsername user, UserUsername !=. username] [LimitTo 5]
            returnJson users
        Nothing -> do
            users <- runDB $ selectList [UserUsername !=. username] [LimitTo 5]
            returnJson users
