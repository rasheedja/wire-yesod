{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Handle search queries from the user. Renders a page displaying a search
-- form and then handles input from the form, forwarding users to the
-- appropriate handler.
module Handler.Search where

import           Form.Search
import           Import

-- | Generate the search form and load the search page template
getSearchR :: Handler Html
getSearchR = do
    (formWidget, formEnctype) <- generateFormPost searchForm
    defaultLayout $ do
        setTitle "Search for Users or Wires"
        $(widgetFile "search")

-- | Handle user input from the search form
postSearchR :: Handler Html
postSearchR = do
    ((result, _), _) <- runFormPost searchForm
    case result of
        FormSuccess searchData -> do
            let Search userQuery userIsWireSearch = searchData
            if userIsWireSearch
                then redirect $ SearchMessageR userQuery
                else redirect $ SearchUsernameR userQuery
        FormFailure errors -> do
            let renderedMessages = map renderErrorMessage errors
            setSession "msgrendered" "true"
            setMessage $ toHtml renderedMessages
            redirect SearchR
        FormMissing -> do
            setSession "msgrendered" "true"
            setMessage $ renderErrorMessage "Form is missing"
            redirect SearchR
