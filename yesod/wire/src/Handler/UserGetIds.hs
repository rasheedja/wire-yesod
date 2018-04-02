{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetIds where


import           Import

-- | Takes in a list of user ids and returns data on all user matching the
-- given list. If no user is found, an empty JSON object is returned.
getUserGetIdsR :: [UserId] -> Handler Value
getUserGetIdsR userIds = do
    users <- runDB $ selectList [UserId <-. userIds] []
    let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
    returnJson cleanUsers
