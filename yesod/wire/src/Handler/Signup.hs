{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Signup where

import Import
import Yesod.Auth.HashDB (setPassword)
import Yesod.Form.Bootstrap3

signupForm :: Form User
signupForm = renderBootstrap3 BootstrapBasicForm $ User
    <$> areq textField (bfs ("Username" :: Text)) Nothing
    <*> areq emailField (bfs ("Email" :: Text)) Nothing
    <*> areq passwordField (bfs ("Passowrd" :: Text)) Nothing

getSignupR :: Handler Html
getSignupR = do
    (formWidget, formEnctype) <- generateFormPost signupForm
    defaultLayout $ do
        setTitle "Wire - Sign Up"
        $(widgetFile "signup")

postSignupR :: Handler Html
postSignupR = do
    ((result, formWidget), formEnctype) <- runFormPost signupForm
    case result of
        FormSuccess user -> do
            -- Do validation here?
            -- userUsername user, see if the username is unique, user the model function UniqueUser?
            -- userEmail user, see if the email is unique, run a query
            void $ runDB . insert =<< setPassword (userPassword user) user
            setMessage "Welcome to Wire"
            redirect HomeR
        _ ->
            redirect SignupR
