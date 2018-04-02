{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Form.Search where

import           Import
import           Yesod.Form.Bootstrap3

data Search = Search
    { query        :: Text
    , isWireSearch :: Bool
    }
    deriving (Show, Typeable)

searchForm :: Form Search
searchForm = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)) $ Search
    <$> areq textField (bfs ("Search" :: Text)) Nothing
    <*> areq (selectField $ optionsPairs [("Wires" :: Text, True), ("Users" :: Text, False)]) "Search for?" Nothing
    <*  bootstrapSubmit (BootstrapSubmit ("Search" :: Text) "btn-primary" [])
