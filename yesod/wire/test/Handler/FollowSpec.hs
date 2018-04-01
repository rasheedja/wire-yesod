{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.FollowSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getFollowR" $ do
        it "asserts no access for anonymous users" $ do
            get $ FollowR "foo"
            statusIs 403

        it "asserts access for authenticated users" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo

            get $ FollowR "foo"
            statusIs 200

        it "asserts that you cannot follow yourself" $ do
            get SignupR
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo

            get $ FollowR "foo"

            bodyContains "false"
            bodyContains "You cannot follow yourself!"

        it "asserts that you cannot follow a non-existant user" $ do
            get SignupR
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo

            get $ FollowR "bar"

            bodyContains "false"
            bodyContains "The user you tried to follow was not found"

        it "asserts that you can follow another user" $ do
            get SignupR
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "bar"
            authenticateAs foo

            get $ FollowR "bar"

            bodyContains "true"
            bodyContains "You have successfully followed bar"

        it "asserts that you can ufollow another user" $ do
            get SignupR
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "bar"
            authenticateAs foo

            get $ FollowR "bar"
            get $ FollowR "bar"

            bodyContains "true"
            bodyContains "You have successfully unfollowed bar"
