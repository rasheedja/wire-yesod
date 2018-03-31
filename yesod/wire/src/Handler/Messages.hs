{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Messages where


import Import
import Data.Aeson

-- Get all the messages posted by the list of given user ids
getMessagesR :: [UserId] -> Handler Value
getMessagesR userIds = do
    messages <- runDB $ selectList [MessageUserId <-. userIds] []
    returnJson messages
