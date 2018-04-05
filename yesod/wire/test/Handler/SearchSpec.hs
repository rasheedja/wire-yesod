{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SearchSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getSearchR" $ do
        it "asserts access for anonymous users" $ do
            get SearchR
            statusIs 200
        it "asserts that the search page includes the search form" $ do
            get SearchR
            htmlCount "#search-form" 1
        it "asserts that the search form looks correct" $ do
            get SearchR
            htmlCount "#search-form label" 2
            htmlCount "#search-form input" 2
            htmlCount "#search-form select" 1
            htmlCount "#search-form button" 1
            htmlAnyContain "#search-form label" "Search Query"
            htmlAnyContain "#search-form label" "Search for?"

    describe "postSearchR" $ do
        it "asserts that you can make a message query" $ do
            get SearchR

            request $ do
                setMethod "POST"
                setUrl SearchR
                addToken
                byLabel "Search Query" "Lorem Ipsum"
                byLabel "Search for?" "1" -- 1 = Wires

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAnyContain "h3" "No message containing Lorem Ipsum found"

        it "asserts that you can make a user query" $ do
            get SearchR

            request $ do
                setMethod "POST"
                setUrl SearchR
                addToken
                byLabel "Search Query" "Lorem Ipsum"
                byLabel "Search for?" "2" -- 2 = Users

            statusIs 303
            _ <- followRedirect
            statusIs 200

            htmlAnyContain "h3" "No user containing Lorem Ipsum in their name found"
