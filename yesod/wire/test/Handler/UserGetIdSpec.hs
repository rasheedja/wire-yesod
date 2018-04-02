{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserGetIdSpec (spec) where

import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getUserGetIdR" $ do
        it "asserts access for anonymous users" $ do
            get $ UserGetIdR $ toSqlKey 1
            statusIs 200

        it "asserts nothing is returned when an invalid id is given" $ do
            get $ UserGetIdR $ toSqlKey 1

            bodyNotContains "username"
            bodyNotContains "id"
            bodyNotContains "email"
            bodyNotContains "password"
            bodyNotContains "1"

        it "asserts correct user data is returned when a valid id is given" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            baz <- createUser "baz" "baz@bar.com" "foo"

            get $ UserGetIdR $ entityKey foo

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
