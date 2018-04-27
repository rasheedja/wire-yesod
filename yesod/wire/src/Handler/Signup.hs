{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Signup where

import           Import
import           Yesod.Auth.HashDB     (setPassword)
import           Yesod.Form.Bootstrap3

-- | Define the signup form and error messages when a user tries to sign up
-- with details that are already taken
signupForm :: Form User
signupForm = renderBootstrap3 BootstrapBasicForm $ User
    <$> areq (checkM uniqueUsername textField) (bfs ("Username" :: Text)) Nothing
    <*> areq (checkM uniqueEmail emailField) (bfs ("Email" :: Text)) Nothing
    <*> areq passwordField (bfs ("Password" :: Text)) Nothing
    where
        uniqueUsername name = checkUserData name ("The username \"" <> name <> "\" is already in use!") $ getBy $ UniqueUser name
        uniqueEmail email = checkUserData email ("The email \"" <> email <>"\" is already in use!") $ getBy $ UniqueEmail email
        checkUserData userData msg = fmap (maybe (Right userData) (const $ Left (msg::Text))) . runDB

-- | Load the signup page
getSignupR :: Handler Html
getSignupR = do
    (formWidget, formEnctype) <- generateFormPost signupForm
    defaultLayout $ do
        setTitle "Wire - Sign Up"
        $(widgetFile "signup")

-- | Take in the search form submitted by the user and create a user
-- if the form is valid or show an appropriate message if there is a problem
postSignupR :: Handler Html
postSignupR = do
    ((result, _), _) <- runFormPost signupForm
    case result of
        FormSuccess user -> do
            _ <- runDB . insert =<< setPassword (userPassword user) user
            setSession "msgrendered" "true"
            setMessage $ renderSuccessMessage $ "Welcome to Wire, " <> userUsername user
            setCreds False $ Creds "hashdb" (userUsername user) []
            redirect HomeR
        FormFailure errors -> do
            let renderedMessages = map renderErrorMessage errors
            setSession "msgrendered" "true"
            setMessage $ toHtml renderedMessages
            redirect SignupR
        FormMissing -> do
            setSession "msgrendered" "true"
            setMessage $ renderErrorMessage "Form is missing"
            redirect SignupR
