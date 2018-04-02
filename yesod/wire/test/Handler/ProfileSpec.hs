{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getProfileR" $ do
        it "asserts access for anonymous users" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            get $ ProfileR "foo"
            statusIs 200

        it "asserts error message is shown when accessing non-existant user's page" $ do
            get $ ProfileR "foo"

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAnyContain ".alert-danger > span" "Username not found"

        it "asserts profile page is correctly rendered for existing users" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            get $ ProfileR "foo"
            statusIs 200

            htmlAnyContain "h3" "foo"
            htmlAnyContain "h3" "Wires"
            htmlAnyContain "h3" "Followers"
            htmlAnyContain "h3" "Following"
            htmlAnyContain "h3" "Other Users"
