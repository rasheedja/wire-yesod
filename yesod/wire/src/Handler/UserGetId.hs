{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetId where


import           Import

-- | Takes in a user id and returns data on the user matching the given id.
-- If no user is found, an empty JSON object is returned.
getUserGetIdR :: UserId -> Handler Value
getUserGetIdR userId = do
    users <- runDB $ selectList [UserId ==. userId] []
    let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
    returnJson cleanUsers
