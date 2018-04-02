{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MyProfileSpec (spec) where

import           Data.List  (head)
import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getMyProfileR" $ do
        it "asserts no access for anonymous users" $ do
            get MyProfileR
            statusIs 403

        it "asserts access for authenticated users" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo

            get MyProfileR
            statusIs 200

        it "asserts that the current profile page looks right" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo

            get MyProfileR
            htmlAnyContain "h3" "Your Page"
            htmlAnyContain "h3" "Your Feed"
            htmlAnyContain "h3" "Followers"
            htmlAnyContain "h3" "Following"
            htmlAnyContain "h3" "Other Users"

    describe "postMyProfileR" $ do
        it "asserts that a new message can be created by an authenticated user" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo
            get MyProfileR

            request $ do
                setMethod "POST"
                setUrl MyProfileR
                addToken
                byLabel "Message" "Lorem Ipsum"

            messages <- runDB $ selectList ([] :: [Filter Message]) []
            assertEq "only one message" 1 $ length messages

            let message = entityVal $ Data.List.head messages

            assertEq "message is Lorem Ipsum" "Lorem Ipsum" $ messageMessage message
            assertEq "message posted by foo" (entityKey foo) $ messageUserId message
