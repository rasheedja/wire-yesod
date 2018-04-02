{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.FollowersSpec (spec) where

import           Database.Persist.Sql (fromSqlKey)
import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getFollowersR" $ do
        it "asserts access for anonymous users" $ do
            get $ FollowersR "foo"
            statusIs 200

        it "asserts correct message is shown for looking up non-existant users" $ do
            get $ FollowersR "foo"
            bodyContains "false"
            bodyContains "The given username was not found"

        it "asserts that the request returns nothing if nobody follows a user" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"

            get $ FollowersR "foo"
            bodyNotContains "followerId"
            bodyNotContains "followingId"

        it "asserts that the request returns the correct data if somebody follows a user" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs bar
            get $ FollowR "foo"

            get $ FollowersR "foo"
            bodyContains "followerId"
            bodyContains "followingId"
            bodyContains $ show $ fromSqlKey $ entityKey bar
            bodyNotContains $ show $ fromSqlKey $ entityKey baz

        it "asserts that the request returns the correct data if multiple people follow a user" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs bar
            get $ FollowR "foo"

            authenticateAs baz
            get $ FollowR "foo"

            get $ FollowersR "foo"
            bodyContains "followerId"
            bodyContains "followingId"
            bodyContains $ show $ fromSqlKey $ entityKey bar
            bodyContains $ show $ fromSqlKey $ entityKey baz
