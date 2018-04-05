{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MessageSpec (spec) where

import           Database.Persist.Sql (fromSqlKey)
import           TestImport


spec :: Spec
spec = withApp $ do
    let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0
    let juneFifteen2017UTCTime = UTCTime (fromGregorian 2017 6 15 :: Day) 0
    let februarySecond2018OnePMUTCTime = UTCTime (fromGregorian 2018 2 2 :: Day) 46800

    describe "getMessageR" $ do
        it "asserts access for anonymous users" $ do
            get $ MessageR "foo"
            statusIs 200

        it "asserts correct message is shown when looking up non-existant users" $ do
            get $ MessageR "foo"
            bodyContains "false"
            bodyContains "The given username was not found"

        it "asserts that a request returns nothing if a user has not posted any messages" $ do
            _ <- createUser "foo" "foo@bar.com" "foo"

            get $ MessageR "foo"
            bodyNotContains "message"
            bodyNotContains "userId"
            bodyNotContains "created"

        it "asserts that a request returns the correct data when a user has posted a single message" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"

            _ <- createMessage "Blablabla" fooId aprilFirst2017UTCTime

            get $ MessageR "foo"
            bodyContains "message"
            bodyContains "userId"
            bodyContains "created"
            bodyContains "Blablabla"
            bodyContains $ show $ fromSqlKey fooId
            bodyContains "2017-04-01"
            bodyContains "00:00:00"

        it "asserts that a request returns the correct data when a user has posted multiple messages" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"

            _ <- createMessage "Blablabla" fooId aprilFirst2017UTCTime
            _ <- createMessage "FooBarBaz" fooId juneFifteen2017UTCTime
            _ <- createMessage"YesodDjango" fooId februarySecond2018OnePMUTCTime

            get $ MessageR "foo"
            bodyContains "message"
            bodyContains "userId"
            bodyContains "created"
            bodyContains "Blablabla"
            bodyContains "FooBarBaz"
            bodyContains "YesodDjango"
            bodyContains $ show $ fromSqlKey fooId
            bodyContains "2017-04-01"
            bodyContains "00:00:00"
            bodyContains "13:00:00"
