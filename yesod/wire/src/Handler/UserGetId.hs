{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.UserGetId where


import           Import

getUserGetIdR :: UserId -> Handler Value
getUserGetIdR userId = do
    user <- runDB $ selectList [UserId ==. userId] []
    returnJson user
