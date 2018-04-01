{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import           Application           (makeFoundation, makeLogWare)
import           ClassyPrelude         as X hiding (Handler, delete, deleteBy)
import           Database.Persist      as X hiding (get)
import           Database.Persist.Sql  (SqlPersistM, connEscapeName, rawExecute,
                                        rawSql, runSqlPersistMPool, unSingle)
import           Foundation            as X
import           Model                 as X
import           Test.Hspec            as X
import           Text.Shakespeare.Text (st)
import           Yesod.Auth            as X
import           Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import           Yesod.Default.Config2 (loadYamlSettings, useEnv)
import           Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: DB [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- Login as user entity. The user entity must have a valid username, email, and
-- password for the login to be successful.
loginAsEntity :: Entity User -> YesodExample App ()
loginAsEntity (Entity _ user) = do
    request $ do
        setMethod "POST"
        setUrl SignupR
        addToken
        byLabel "Username" $ userUsername user
        byLabel "Email" $ userEmail user
        byLabel "Password" $ userPassword user

-- Login using the given credentials. The login will only succeed if the
-- credentials are valid
loginAsCreds :: Text -> Text -> Text -> YesodExample App ()
loginAsCreds username email password = do
    request $ do
        setMethod "POST"
        setUrl SignupR
        addToken
        byLabel "Username" username
        byLabel "Email" email
        byLabel "Password" password

-- Create a user with a given username, email, and password.
-- The user is inserted directly into the database and the
-- user entity is returned
createUser :: Text -> Text -> Text -> YesodExample App (Entity User)
createUser username email password = runDB $ do
    user <- insertEntity $ User username email password
    return user
