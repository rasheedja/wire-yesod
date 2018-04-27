{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Helper.Message
import           Import

-- | Load the latest messages posted by the user and then render the home page
getHomeR :: Handler Html
getHomeR = do
    latestMessages <- runDB $ selectList [] [Desc MessageCreated, LimitTo 5]
    let posterIds = Import.map (\(Entity _ (Message _ posterId _)) -> posterId) latestMessages
    posters <- runDB $ selectList [UserId <-. posterIds] []

    latestTaggedMessages <- runDB $ selectList
        (
            [Filter MessageMessage (Left $ Import.concat ["% #%"]) (BackendSpecificFilter "ILIKE")]
            ||. [Filter MessageMessage (Left $ Import.concat ["#%"]) (BackendSpecificFilter "ILIKE")]
        ) [Desc MessageCreated, LimitTo 5]
    let taggedPosterIds = Import.map (\(Entity _ (Message _ posterId _)) -> posterId) latestTaggedMessages
    taggedPosters <- runDB $ selectList [UserId <-. taggedPosterIds] []

    let formattedLatestMessages = Import.map (formatMessageEntity posters) latestMessages
    let formattedTaggedMessages = Import.map (formatMessageEntity taggedPosters) latestTaggedMessages

    defaultLayout $ do
        setTitle "Welcome To Wire!"
        $(widgetFile "homepage")
