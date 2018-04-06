{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Helper.Message where

import qualified Data.List as L
import           Import

-- | A datatype which is useful for displaying the contents
-- of message entities in templates.
data MessageInfo = MessageInfo
    { message  :: Text
    , username :: Text
    , created  :: [Char]
    }
    deriving (Show, Typeable)

-- | Populates the MessageInfo datatype. This is done by using
-- the message from the message entity, formatting the date from
-- the message entity, and finding the user entity using the user id
-- from the message entity and running that id against a list of
-- given users.
formatMessageEntity :: [Entity User] -> Entity Message -> MessageInfo
formatMessageEntity users (Entity _ (Message posterMessage userId postCreated)) = do
    let formattedTime = formatTime defaultTimeLocale "%d/%m/%Y at %H:%M" postCreated
    -- let maybeUserId = Import.lookup userId users
    let maybeUserIndex = L.elemIndex userId $ L.map entityKey users
    case maybeUserIndex of
        Just userIndex -> do
            let posterName = userUsername $ entityVal (users L.!! userIndex)
            MessageInfo posterMessage posterName formattedTime
        Nothing -> MessageInfo posterMessage "unknown" formattedTime
