{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SearchMessageSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
    let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0
    let februaryTwentyFirst2017UTCTime = UTCTime (fromGregorian 2017 2 21 :: Day) 0
    let januaryThirtieth2019OnePMUTCTime = UTCTime (fromGregorian 2018 1 30 :: Day) 46800

    describe "getSearchMessageR" $ do
        it "asserts access for anonymous users" $ do
            get $ SearchMessageR "foo"
            statusIs 200

        it "asserts that the results looks right when there are no results" $ do
            get $ SearchMessageR "foo"
            htmlAnyContain "h3" "No message containing foo found"
            htmlNoneContain "h3" "Wire"

        it "asserts that the results page looks right when a single message is found" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createMessage "foo" (entityKey foo) aprilFirst2017UTCTime

            get $ SearchMessageR "foo"

            htmlAnyContain "h3" "Wire"
            htmlNoneContain "h3" "No message containing foo found"

            htmlAnyContain ".list-group-item" "foo"
            htmlNoneContain ".list-group-item" "bar"
            htmlNoneContain ".list-group-item" "baz"

        it "asserts that the results page looks correct when multiple messages are found" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"

            _ <- createMessage "foo" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "foo_bar" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "foo_bar_baz" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "baz_bar" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "baz_baz" (entityKey foo) aprilFirst2017UTCTime

            get $ SearchMessageR "foo"

            htmlAnyContain "h3" "Wire"
            htmlNoneContain "h3" "No message containing foo found"

            htmlAnyContain ".list-group-item" "foo"
            htmlAnyContain ".list-group-item" "foo_bar"
            htmlAnyContain ".list-group-item" "foo_bar_baz"
            htmlNoneContain ".list-group-item" "baz_bar"
            htmlNoneContain ".list-group-item" "baz_baz"

        it "asserts that the results display the correct user that posted a message" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            bar <- createUser "bar" "bar@bar.com" "foo"
            _ <- createUser "baz" "baz@bar.com" "foo"

            _ <- createMessage "foo 1" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "foo 2" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "foo 3" (entityKey bar) aprilFirst2017UTCTime

            get $ SearchMessageR "foo"

            htmlAnyContain "h3" "Wire"
            htmlNoneContain "h3" "No user containing foo in their name found"

            htmlAnyContain ".list-group-item-text" "foo"
            htmlAnyContain ".list-group-item-text" "bar"
            htmlNoneContain ".list-group-item-text" "baz"

        it "asserts that the results displays the correct formatted date for each message" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"

            _ <- createMessage "foo 1" (entityKey foo) aprilFirst2017UTCTime
            _ <- createMessage "foo 2" (entityKey foo) februaryTwentyFirst2017UTCTime
            _ <- createMessage "foo 3" (entityKey foo) januaryThirtieth2019OnePMUTCTime

            get $ SearchMessageR "foo"

            htmlAnyContain "h3" "Wire"
            htmlNoneContain "h3" "No user containing foo in their name found"

            htmlAnyContain ".list-group-item-text" "01/04/2017 at 00:00"
            htmlAnyContain ".list-group-item-text" "21/02/2017 at 00:00"
            htmlAnyContain ".list-group-item-text" "30/01/2018 at 13:00"
