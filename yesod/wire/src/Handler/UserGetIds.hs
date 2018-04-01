{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetIds where


import           Import

getUserGetIdsR :: [UserId] -> Handler Value
getUserGetIdsR userIds = do
    users <- runDB $ selectList [UserId <-. userIds] []
    returnJson users
