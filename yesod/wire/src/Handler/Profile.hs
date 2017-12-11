{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    maybeUsername <- lookupGetParam "username"
    case maybeUsername of
        Just username -> do
            maybeUser <- runDB $ getBy $ UniqueUser username
            case maybeUser of
                Just (Entity userId user) ->
                    defaultLayout $ do
                        setTitle . toHtml $ userUsername user
                        $(widgetFile "profile")
                Nothing -> do
                    defaultLayout $ do
                        -- TODO: Redirect to user search page
                        setSession "msgrendered" "true"
                        setMessage $ renderErrorMessage "Invalid Username"
                        redirect HomeR
        Nothing -> do
            maybeUser <- maybeAuth
            case maybeUser of
                Just (Entity userId user) ->
                    defaultLayout $ do
                        setTitle . toHtml $ userUsername user
                        $(widgetFile "profile")
                Nothing -> do
                    -- TODO: Redirect to user search page
                    defaultLayout $ do
                        setSession "msgrendered" "true"
                        setMessage $ renderErrorMessage "Please enter a username"
                        redirect HomeR
