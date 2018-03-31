{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.MyProfile where

import Import
import Yesod.Form.Bootstrap3
import Text.Julius

messageForm :: UserId -> Form Message
messageForm userId = renderBootstrap3 BootstrapBasicForm $ Message
    <$> areq textField (bfs ("Message" :: Text)) Nothing
    <*> pure userId
    <*> lift (liftIO getCurrentTime)

-- Loads the 'My Profile' page for the currently logged in user. If someone
-- who is not logged in attempts to access the page, a 404 error will be shown.
getMyProfileR :: Handler Html
getMyProfileR = do
    (Entity userId user) <- requireAuth

    let username = userUsername user
    let jsUsername = rawJS username

    -- Load messages posted by users followed by the current user
    follows <- runDB $ selectList [FollowFollowingId ==. userId] []
    let followIds = map (\(Entity _ (Follow followIds _)) -> followIds) follows

    (formWidget, formEnctype) <- generateFormPost $ messageForm userId
    defaultLayout $ do
        setTitle . toHtml $ userUsername user
        $(widgetFile "currentprofile")

-- Create a new wire for the logged in user
postMyProfileR :: Handler Html
postMyProfileR = do
    (Entity userId _) <- requireAuth
    ((result, _), _) <- runFormPost $ messageForm userId
    case result of
        FormSuccess message -> do
            void $ runDB . insert $ message
            setSession "msgrendered" "true"
            setMessage $ renderSuccessMessage $ "Wire Sent"
            redirect MyProfileR
        FormFailure errors -> do
            let renderedMessages = map renderErrorMessage errors
            setSession "msgrendered" "true"
            setMessage $ toHtml renderedMessages
            redirect MyProfileR
        FormMissing -> do
            setSession "msgrendered" "true"
            setMessage $ renderErrorMessage "Form is missing"
            redirect MyProfileR
