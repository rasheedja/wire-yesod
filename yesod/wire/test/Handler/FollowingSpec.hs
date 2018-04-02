{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.FollowingSpec (spec) where

import           Database.Persist.Sql (fromSqlKey)
import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getFollowingR" $ do
        it "asserts access for anonymous users" $ do
            get $ FollowingR "foo"
            statusIs 200

        it "asserts correct message is shown when looking up non-existant users" $ do
            get $ FollowingR "foo"
            bodyContains "false"
            bodyContains "The given username was not found"

        it "asserts that the request returns nothing if a user is not following anyone" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"

            get $ FollowingR "foo"
            bodyNotContains "followerId"
            bodyNotContains "followingId"

        it "asserts that the request returns the correct data if a user follows someone" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo
            get $ FollowR "bar"

            get $ FollowingR "foo"
            bodyContains "followerId"
            bodyContains "followingId"
            bodyContains $ show $ fromSqlKey $ entityKey bar
            bodyNotContains $ show $ fromSqlKey $ entityKey baz

        it "asserts that the request returns the correct data if a user follows multiple people" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo
            get $ FollowR "bar"
            get $ FollowR "baz"

            get $ FollowingR "foo"
            bodyContains "followerId"
            bodyContains "followingId"
            bodyContains $ show $ fromSqlKey $ entityKey bar
            bodyContains $ show $ fromSqlKey $ entityKey baz
