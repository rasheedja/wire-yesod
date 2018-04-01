{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Messages where


import           Data.Aeson
import           Import

-- Get all the messages posted by the list of given user ids
getMessagesR :: [UserId] -> Handler Value
getMessagesR userIds = do
    messages <- runDB $ selectList [MessageUserId <-. userIds] []
    returnJson messages
