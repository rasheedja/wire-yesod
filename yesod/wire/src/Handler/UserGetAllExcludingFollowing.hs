{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetAllExcludingFollowing where


import           Import

-- Return all users that aren't following the current user
getUserGetAllExcludingFollowingR :: Handler Value
getUserGetAllExcludingFollowingR = do
    Entity userId user <- requireAuth
    followers <- runDB $ selectList [FollowFollowerId ==. userId] []
    -- See: https://stackoverflow.com/questions/36727794/haskell-persistent-reusing-selectlist
    let followingIds = map (\(Entity _ (Follow _ followingId)) -> followingId) followers
    users <- runDB $ selectList [UserUsername !=. userUsername user, UserId /<-. followingIds] [LimitTo 5]
    returnJson users
