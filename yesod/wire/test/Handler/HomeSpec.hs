{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import           Data.Time.Clock (NominalDiffTime, addUTCTime)
import           TestImport

spec :: Spec
spec = withApp $ do
    let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0

    describe "getHomeR" $ do
        it "asserts access for anonymous users" $ do
            get $ HomeR
            statusIs 200

        it "loads the index and checks it looks right" $ do
            get HomeR
            statusIs 200
            htmlAnyContain "h1" "Latest Wires"
            htmlAnyContain "h1" "Popular Tags"

        -- This is a simple example of using a database access in a test.  The
        -- test will succeed for a fresh scaffolded site with an empty database,
        -- but will fail on an existing database with a non-empty user table.
        it "leaves the user table empty" $ do
            get HomeR
            statusIs 200
            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEq "user table empty" 0 $ length users

        it "checks that an appropriate message is shown if there are no messages" $ do
            get HomeR
            htmlAnyContain "h3" "No one has created a Wire yet"

        it "checks that messages are shown if they exist" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createMessage "test message" (entityKey foo) aprilFirst2017UTCTime

            get HomeR
            htmlAnyContain "#latest-messages .list-group-item" "test message"

        it "checks that the latest messages section display the correct user in the footer" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createUser "bar" "bar@bar.com" "bar"
            _ <- createMessage "test message" (entityKey foo) aprilFirst2017UTCTime

            get HomeR
            htmlAnyContain "#latest-messages .list-group-item-text" "foo"
            htmlNoneContain "#latest-messages .list-group-item-text" "bar"

        it "checks that the latest messages section display the correct date in the footer" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createMessage "test message" (entityKey foo) aprilFirst2017UTCTime

            get HomeR
            htmlAnyContain "#latest-messages .list-group-item-text" "01/04/2017"
            htmlAnyContain "#latest-messages .list-group-item-text" "00:00"

        it "asserts that only the five latest messages are displayed" $ do
            foo <- createUser "foo" "foo@bar.com" "foo"
            _ <- createMessage "test message 1" (entityKey foo) $ addUTCTime (5 :: NominalDiffTime) aprilFirst2017UTCTime
            _ <- createMessage "test message 2" (entityKey foo) $ addUTCTime (15 :: NominalDiffTime) aprilFirst2017UTCTime
            _ <- createMessage "test message 3" (entityKey foo) $ addUTCTime (25 :: NominalDiffTime) aprilFirst2017UTCTime
            _ <- createMessage "test message 4" (entityKey foo) $ addUTCTime (35 :: NominalDiffTime) aprilFirst2017UTCTime
            _ <- createMessage "test message 5" (entityKey foo) $ addUTCTime (45 :: NominalDiffTime) aprilFirst2017UTCTime
            _ <- createMessage "test message 6" (entityKey foo) $ addUTCTime (55 :: NominalDiffTime) aprilFirst2017UTCTime
            _ <- createMessage "test message 7" (entityKey foo) $ addUTCTime (65 :: NominalDiffTime) aprilFirst2017UTCTime

            get HomeR
            htmlCount "#latest-messages .list-group-item" 5
            htmlNoneContain "#latest-messages .list-group-item" "test message 1"
            htmlNoneContain "#latest-messages .list-group-item" "test message 2"
            htmlAnyContain "#latest-messages .list-group-item" "test message 3"
            htmlAnyContain "#latest-messages .list-group-item" "test message 4"
            htmlAnyContain "#latest-messages .list-group-item" "test message 5"
            htmlAnyContain "#latest-messages .list-group-item" "test message 6"
            htmlAnyContain "#latest-messages .list-group-item" "test message 7"
