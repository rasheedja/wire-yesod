{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetAllExcludingFollowing where


import           Import

-- | Return all users that aren't followed by the current user
-- Does not return the password or email address of the users
getUserGetAllExcludingFollowingR :: Handler Value
getUserGetAllExcludingFollowingR = do
    Entity userId user <- requireAuth
    followers <- runDB $ selectList [FollowFollowerId ==. userId] []
    -- See: https://stackoverflow.com/questions/36727794/haskell-persistent-reusing-selectlist
    let followingIds = map (\(Entity _ (Follow _ followingId)) -> followingId) followers
    users <- runDB $ selectList [UserUsername !=. userUsername user, UserId /<-. followingIds] [LimitTo 5]
    let cleanUsers = map (\(Entity uid (User uname _ _)) -> (object ["id" .= uid, "username" .= uname])) users
    returnJson cleanUsers
