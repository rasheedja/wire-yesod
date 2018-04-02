{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserGetAllExcludingFollowingSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getUserGetAllExcludingFollowingR" $ do
        it "asserts no access for anonymous users" $ do
            get UserGetAllExcludingFollowingR
            statusIs 403

        it "asserts access for authorized users" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            authenticateAs foo

            get UserGetAllExcludingFollowingR
            statusIs 200


        it "asserts all other users are returned when no one is following the authenticated user" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo
            get UserGetAllExcludingFollowingR

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyContains "bar"
            bodyContains "baz"

        it "asserts only non-followers are returned when one user follows the authenticated user" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo
            get $ FollowR "bar"

            get UserGetAllExcludingFollowingR

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyNotContains "bar"
            bodyContains "baz"

        it "asserts only non-followers are returned when multiple users follow the authenticated user" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"
            _ <- createUser "ben" "ben@bar.com" "foo"
            _ <- createUser "ten" "ten@bar.com" "foo"

            authenticateAs foo
            get $ FollowR "bar"
            get $ FollowR "baz"


            get UserGetAllExcludingFollowingR

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyNotContains "bar"
            bodyNotContains "baz"
            bodyContains "ben"
            bodyContains "ten"

        it "asserts nothing is returned when every users follow the authenticated user" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo
            get $ FollowR "bar"
            get $ FollowR "baz"


            get UserGetAllExcludingFollowingR

            bodyNotContains "username"
            bodyNotContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyNotContains "bar"
            bodyNotContains "baz"
