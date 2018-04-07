{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Helper.Message
import           Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    latestMessages <- runDB $ selectList [] [Desc MessageCreated, LimitTo 5]
    let posterIds = Import.map (\(Entity _ (Message _ posterId _)) -> posterId) latestMessages
    posters <- runDB $ selectList [UserId <-. posterIds] []

    latestTaggedMessages <- runDB $ selectList [Filter MessageMessage (Left $ Import.concat ["% #%"]) (BackendSpecificFilter "ILIKE")] [Desc MessageCreated, LimitTo 5]
    let taggedPosterIds = Import.map (\(Entity _ (Message _ posterId _)) -> posterId) latestTaggedMessages
    taggedPosters <- runDB $ selectList [UserId <-. taggedPosterIds] []

    let formattedLatestMessages = Import.map (formatMessageEntity posters) latestMessages
    let formattedTaggedMessages = Import.map (formatMessageEntity taggedPosters) latestTaggedMessages

    defaultLayout $ do
        setTitle "Welcome To Wire!"
        $(widgetFile "homepage")
