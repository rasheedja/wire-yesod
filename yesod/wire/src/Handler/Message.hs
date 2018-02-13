{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Message where


import Import
import Data.Aeson

getMessageR :: Handler Value
getMessageR = do
    maybeUsername <- lookupGetParam "username"
    case maybeUsername of
        Just username -> do
            maybeUser <- runDB $ getBy $ UniqueUser username
            case maybeUser of
                Just (Entity userId user) -> do
                    messages <- runDB $ selectList [MessageUserId ==. userId] []
                    returnJson messages
                Nothing -> do
                    returnJson $ object []
        Nothing -> do
            returnJson $ object []

