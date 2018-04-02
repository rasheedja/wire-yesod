{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MessagesSpec (spec) where

import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           TestImport


spec :: Spec
spec = withApp $ do

    describe "getMessagesR" $ do
        it "asserts access for anonymous users" $ do
            get $ MessagesR $ [toSqlKey 1]
            statusIs 200

        it "asserts correct message is shown when looking up non-existant users" $ do
            get $ MessagesR $ [toSqlKey 1, toSqlKey 2, toSqlKey 3]
            bodyNotContains "message"
            bodyNotContains "userId"
            bodyNotContains "created"

        it "asserts that a request returns nothing if a single user has not posted any messages" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"

            get $ MessagesR [fooId]
            bodyNotContains "message"
            bodyNotContains "userId"
            bodyNotContains "created"

        it "asserts that a request returns the correct data when a single user has posted a single message" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"

            let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0

            _ <- runDB $ insert $ Message "Blablabla" fooId aprilFirst2017UTCTime

            get $ MessagesR [fooId]
            bodyContains "message"
            bodyContains "userId"
            bodyContains "created"
            bodyContains "Blablabla"
            bodyContains $ show $ fromSqlKey fooId
            bodyContains "2017-04-01"
            bodyContains "00:00:00"

        it "asserts that a request returns the correct data when a single user has posted multiple messages" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"

            let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0
            let juneFifteen2017UTCTime = UTCTime (fromGregorian 2017 6 15 :: Day) 0
            let februarySecond2018OnePMUTCTime = UTCTime (fromGregorian 2018 2 2 :: Day) 46800

            _ <- runDB $ insert $ Message "Blablabla" fooId aprilFirst2017UTCTime
            _ <- runDB $ insert $ Message "FooBarBaz" fooId juneFifteen2017UTCTime
            _ <- runDB $ insert $ Message "YesodDjango" fooId februarySecond2018OnePMUTCTime

            get $ MessagesR [fooId]
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

        it "asserts that a request returns the correct data when multiple users have posted a single message" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"
            (Entity barId _) <- createUser "bar" "bar@bar.com" "foo"
            (Entity bazId _) <- createUser "baz" "baz@bar.com" "foo"

            let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0
            let juneFifteen2017UTCTime = UTCTime (fromGregorian 2017 6 15 :: Day) 0
            let februarySecond2018OnePMUTCTime = UTCTime (fromGregorian 2018 2 2 :: Day) 46800

            _ <- runDB $ insert $ Message "Blablabla" fooId aprilFirst2017UTCTime
            _ <- runDB $ insert $ Message "FooBarBaz" barId juneFifteen2017UTCTime
            _ <- runDB $ insert $ Message "YesodDjango" bazId februarySecond2018OnePMUTCTime

            get $ MessagesR [fooId, barId, bazId]
            bodyContains "message"
            bodyContains "userId"
            bodyContains "created"
            bodyContains "Blablabla"
            bodyContains "FooBarBaz"
            bodyContains "YesodDjango"
            bodyContains $ show $ fromSqlKey fooId
            bodyContains $ show $ fromSqlKey barId
            bodyContains $ show $ fromSqlKey bazId
            bodyContains "2017-04-01"
            bodyContains "00:00:00"
            bodyContains "13:00:00"

        it "asserts that a request returns the correct data when multiple users have posted multiple message" $ do
            (Entity fooId _) <- createUser "foo" "foo@bar.com" "foo"
            (Entity barId _) <- createUser "bar" "bar@bar.com" "foo"

            let aprilFirst2017UTCTime = UTCTime (fromGregorian 2017 4 1 :: Day) 0
            let juneFifteen2017UTCTime = UTCTime (fromGregorian 2017 6 15 :: Day) 0
            let februarySecond2018OnePMUTCTime = UTCTime (fromGregorian 2018 2 2 :: Day) 46800

            _ <- runDB $ insert $ Message "Blablabla" fooId aprilFirst2017UTCTime
            _ <- runDB $ insert $ Message "FooBarBaz" fooId juneFifteen2017UTCTime
            _ <- runDB $ insert $ Message "YesodDjango" barId februarySecond2018OnePMUTCTime
            _ <- runDB $ insert $ Message "DjangoYesod" barId februarySecond2018OnePMUTCTime

            get $ MessagesR [fooId, barId]
            bodyContains "message"
            bodyContains "userId"
            bodyContains "created"
            bodyContains "Blablabla"
            bodyContains "FooBarBaz"
            bodyContains "YesodDjango"
            bodyContains "DjangoYesod"
            bodyContains $ show $ fromSqlKey fooId
            bodyContains $ show $ fromSqlKey barId
            bodyContains "2017-04-01"
            bodyContains "00:00:00"
            bodyContains "13:00:00"
