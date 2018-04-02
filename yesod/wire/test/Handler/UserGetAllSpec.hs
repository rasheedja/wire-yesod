{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserGetAllSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getUserGetAllR" $ do
        it "asserts access for anonymous users" $ do
            get UserGetAllR
            statusIs 200

        it "asserts all users are returned when not authenticated" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            get UserGetAllR

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyContains "foo"
            bodyContains "bar"
            bodyContains "baz"

        it "asserts all users are returned except authenticated user" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo

            get UserGetAllR

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyContains "bar"
            bodyContains "baz"
