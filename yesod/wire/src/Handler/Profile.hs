{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3
import Text.Julius

messageForm :: UserId -> Form Message
messageForm userId = renderBootstrap3 BootstrapBasicForm $ Message
    <$> areq textField (bfs ("Message" :: Text)) Nothing
    <*> pure userId
    <*> lift (liftIO getCurrentTime)

-- Look up the 'username' GET parameter. If one is given, check if the username
-- exists in the database. If it is a valid username, render the given user's
-- profile page. If no username is given, render the current users profile page
-- if a user is logged in. If the rendered profile page is the page of the
-- logged in user, allow the user to create new wires on the page. If an invalid
-- username is given, redirect the user to the user search page
getProfileR :: Handler Html
getProfileR = do
    maybeUsername <- lookupGetParam "username"
    case maybeUsername of
        Just username -> do
            let jsUsername = rawJS username
            maybeUser <- runDB $ getBy $ UniqueUser username
            case maybeUser of
                Just (Entity userId user) -> do
                    maybeLoggedInUser <- maybeAuth
                    case maybeLoggedInUser of
                        Just (Entity loggedInUserId _) -> do
                            if loggedInUserId == userId
                                then do
                                    (formWidget, formEnctype) <- generateFormPost $ messageForm userId
                                    defaultLayout $ do
                                        setTitle . toHtml $ userUsername user
                                        $(widgetFile "currentprofile")
                                else
                                    defaultLayout $ do
                                        setTitle . toHtml $ userUsername user
                                        $(widgetFile "profile")
                        Nothing -> do
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
                Just (Entity userId user) -> do
                    let jsUsername = rawJS $ userUsername user
                    (formWidget, formEnctype) <- generateFormPost $ messageForm userId
                    defaultLayout $ do
                        setTitle . toHtml $ userUsername user
                        $(widgetFile "currentprofile")
                Nothing -> do
                    -- TODO: Redirect to user search page
                    defaultLayout $ do
                        setSession "msgrendered" "true"
                        setMessage $ renderErrorMessage "Please enter a username"
                        redirect HomeR

-- Create a new wire for the logged in user
postProfileR :: Handler Html
postProfileR = do
    (Entity userId _) <- requireAuth
    ((result, _), _) <- runFormPost $ messageForm userId
    case result of
        FormSuccess message -> do
            void $ runDB . insert $ message
            setSession "msgrendered" "true"
            setMessage $ renderSuccessMessage $ "Wire Sent"
            redirect ProfileR
        FormFailure errors -> do
            let renderedMessages = map renderErrorMessage errors
            setSession "msgrendered" "true"
            setMessage $ toHtml renderedMessages
            redirect ProfileR
        FormMissing -> do
            setSession "msgrendered" "true"
            setMessage $ renderErrorMessage "Form is missing"
            redirect ProfileR
