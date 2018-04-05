{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SearchUsernameSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getSearchUsernameR" $ do
        it "asserts access for anonymous users" $ do
            get $ SearchUsernameR "foo"
            statusIs 200

        it "asserts that the results looks right when there are no results" $ do
            get $ SearchUsernameR "foo"
            htmlAnyContain "h3" "No user containing foo in their name found"
            htmlNoneContain "h3" "User"

        it "asserts that the results page looks right when a single username is found" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            get $ SearchUsernameR "foo"

            htmlAnyContain "h3" "User"
            htmlNoneContain "h3" "No user containing foo in their name found"

            htmlAnyContain ".list-group-item" "foo"
            htmlNoneContain ".list-group-item" "bar"
            htmlNoneContain ".list-group-item" "baz"

        it "asserts that the results page looks correct when multiple usernames are found" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "foo_bar" "foo_bar@bar.com" "foo"
            _ <- createUser "foo_bar_baz" "foo_bar_baz@bar.com" "foo"
            _ <- createUser "baz_bar" "bar@bar.com" "foo"
            _ <- createUser "baz_baz" "baz@bar.com" "foo"

            get $ SearchUsernameR "foo"

            htmlAnyContain "h3" "User"
            htmlNoneContain "h3" "No user containing foo in their name found"

            htmlAnyContain ".list-group-item" "foo"
            htmlAnyContain ".list-group-item" "foo_bar"
            htmlAnyContain ".list-group-item" "foo_bar_baz"
            htmlNoneContain ".list-group-item" "baz_bar"
            htmlNoneContain ".list-group-item" "baz_baz"
