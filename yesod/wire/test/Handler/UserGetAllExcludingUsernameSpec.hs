{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserGetAllExcludingUsernameSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getUserGetAllExcludingUsernameR" $ do
        it "asserts access for anonymous users" $ do
            get $ UserGetAllExcludingUsernameR "foo"
            statusIs 200


        it "asserts all other users apart from the specified user are returned" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            get $ UserGetAllExcludingUsernameR "foo"

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyContains "bar"
            bodyContains "baz"

        it "asserts all other users apart from the specified and the authenticated users are returned" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            authenticateAs foo
            get $ UserGetAllExcludingUsernameR "bar"

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyNotContains "bar"
            bodyContains "baz"

        it "asserts a maximum of five users are returned when not authenticated" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"
            _ <- createUser "ben" "ben@bar.com" "foo"
            _ <- createUser "ten" "ten@bar.com" "foo"
            _ <- createUser "snk" "snk@bar.com" "foo"
            _ <- createUser "lqd" "lqd@bar.com" "foo"
            _ <- createUser "sld" "sld@bar.com" "foo"

            get $ UserGetAllExcludingUsernameR "bar"

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyContains "foo"
            bodyNotContains "bar"
            bodyContains "baz"
            bodyContains "ben"
            bodyContains "ten"
            bodyContains "snk"
            bodyNotContains "lqd"
            bodyNotContains "sld"

        it "asserts a maximum of five users are returned when authenticated" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"
            _ <- createUser "ben" "ben@bar.com" "foo"
            _ <- createUser "ten" "ten@bar.com" "foo"
            _ <- createUser "snk" "snk@bar.com" "foo"
            _ <- createUser "lqd" "lqd@bar.com" "foo"
            _ <- createUser "sld" "sld@bar.com" "foo"

            authenticateAs foo
            get $ UserGetAllExcludingUsernameR "bar"

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "foo"
            bodyNotContains "bar"
            bodyContains "baz"
            bodyContains "ben"
            bodyContains "ten"
            bodyContains "snk"
            bodyContains "lqd"
            bodyNotContains "sld"
