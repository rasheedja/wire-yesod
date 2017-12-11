{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Signup where

import Import
import Yesod.Auth.HashDB (setPassword)
import Yesod.Form.Bootstrap3

signupForm :: Form User
signupForm = renderBootstrap3 BootstrapBasicForm $ User
    <$> areq (checkM uniqueUsername textField) (bfs ("Username" :: Text)) Nothing
    <*> areq (checkM uniqueEmail emailField) (bfs ("Email" :: Text)) Nothing
    <*> areq passwordField (bfs ("Passowrd" :: Text)) Nothing
    where
        uniqueUsername name = checkUserData name ("The username \"" <> name <> "\" is already in use!") $ getBy $ UniqueUser name
        uniqueEmail email = checkUserData email ("The email \"" <> email <>"\" is already in use!") $ getBy $ UniqueEmail email
        checkUserData userData msg = liftM (maybe (Right userData) (const $ Left (msg::Text))) . runDB

getSignupR :: Handler Html
getSignupR = do
    (formWidget, formEnctype) <- generateFormPost signupForm
    defaultLayout $ do
        setTitle "Wire - Sign Up"
        setSession "msgrendered" "true"
        $(widgetFile "signup")

postSignupR :: Handler Html
postSignupR = do
    ((result, formWidget), formEnctype) <- runFormPost signupForm
    case result of
        FormSuccess user -> do
            void $ runDB . insert =<< setPassword (userPassword user) user
            setMessage $ renderSuccessMessage $ "Welcome to Wire, " <> (userUsername user)
            redirect HomeR
        FormFailure errors -> do
            let renderedMessages = map renderErrorMessage errors
            setMessage $ toHtml renderedMessages
            redirect SignupR
        FormMissing -> do
            setMessage $ renderErrorMessage "Form is missing"
            redirect SignupR
