{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SignupSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Signup page" $ do
        it "checks the signup page looks right" $ do
            get SignupR
            statusIs 200
            htmlCount "form" 1
            htmlCount "input.form-control" 3
            htmlAnyContain "label" "Username"
            htmlAnyContain "label" "Password"
            htmlAnyContain "label" "Email"

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

            htmlAnyContain ".alert-danger > span" "Value is required"
            htmlCount ".alert-danger > span" 3
