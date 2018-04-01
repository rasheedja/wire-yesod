{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SignupSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getSignupR" $ do
        it "checks the signup page contains the signup form" $ do
            get SignupR
            statusIs 200
            htmlCount "form" 1
            htmlCount "input.form-control" 3
            htmlAnyContain "label" "Username"
            htmlAnyContain "label" "Password"
            htmlAnyContain "label" "Email"

    describe "postSignupR" $ do
        it "redirects to signup page with messages when no input is given" $ do
            get SignupR
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl SignupR
                addToken
                byLabel "Username" ""
                byLabel "Email" ""
                byLabel "Password" ""

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAllContain ".alert-danger > span" "Value is required"
            htmlCount ".alert-danger > span" 3

            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEq "user table has no entries" 0 $ length users

        it "creates a new user and checks if we are redirected correctly" $ do
            get SignupR
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl SignupR
                addToken
                byLabel "Username" "foo"
                byLabel "Email" "foo@bar.com"
                byLabel "Password" "foo"

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAnyContain ".alert-success > span" "Welcome to Wire, foo"
            htmlCount ".alert-success > span" 1

            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEq "user table has one entry" 1 $ length users

        it "tries to create a user using a username and email that has already been taken" $ do
            _ <- runDB $ insert $ User "foo" "foo@bar.com" "foo"

            get SignupR
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl SignupR
                addToken
                byLabel "Username" "foo"
                byLabel "Email" "foo@bar.com"
                byLabel "Password" "foo"

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAnyContain ".alert-danger > span" "The username"
            htmlAnyContain ".alert-danger > span" "The email"
            htmlAnyContain ".alert-danger > span" "foo"
            htmlAnyContain ".alert-danger > span" "foo@bar.com"
            htmlAllContain ".alert-danger > span" "is already in use!"
            htmlCount ".alert-danger > span" 2

            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEq "user table has one entry" 1 $ length users

        it "tries to create a user using an invalid email address" $ do
            get SignupR
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl SignupR
                addToken
                byLabel "Username" "foo"
                byLabel "Email" "foo"
                byLabel "Password" "foo"

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAnyContain ".alert-danger > span" "Invalid e-mail address: foo"
            htmlCount ".alert-danger > span" 1

            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEq "user table has no entries" 0 $ length users
