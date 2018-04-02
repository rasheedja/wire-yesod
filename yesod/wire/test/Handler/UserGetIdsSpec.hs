{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserGetIdsSpec (spec) where

import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getUserGetIdsR" $ do
        it "asserts access for anonymous users" $ do
            get $ UserGetIdsR $ [toSqlKey 1]
            statusIs 200

        it "asserts nothing is returned when a single invalid id is given" $ do
            get $ UserGetIdsR $ [toSqlKey 1]

            bodyNotContains "username"
            bodyNotContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "1"

        it "asserts nothing is returned when a multiple invalid ids are given" $ do
            get $ UserGetIdsR $ [toSqlKey 1, toSqlKey 2, toSqlKey 3]

            bodyNotContains "username"
            bodyNotContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "1"
            bodyNotContains "2"
            bodyNotContains "3"

        it "asserts correct user data is returned when a single valid id is given" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            get $ UserGetIdsR $ [entityKey foo]

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyContains "foo"
            bodyContains $ show $ fromSqlKey (entityKey foo)
            bodyNotContains "bar"
            bodyNotContains $ show $ fromSqlKey (entityKey bar)
            bodyNotContains "baz"
            bodyNotContains $ show $ fromSqlKey (entityKey baz)

        it "asserts correct user data is returned when multiple ids are given" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            get $ UserGetIdsR $ [entityKey foo, entityKey bar]

            bodyContains "username"
            bodyContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyContains "foo"
            bodyContains $ show $ fromSqlKey (entityKey foo)
            bodyContains "bar"
            bodyContains $ show $ fromSqlKey (entityKey bar)
            bodyNotContains "baz"
            bodyNotContains $ show $ fromSqlKey (entityKey baz)
