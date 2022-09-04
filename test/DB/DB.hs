
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module DB.DB where

import           Conduit                          (MonadIO (..), MonadResource)
import           Configuration.Dotenv             (Config (Config), loadFile)
import           Configuration.Dotenv.Environment (lookupEnv)
import           Control.Lens                     (Each (each), makeLenses,
                                                   over, traverseOf, (&), (<&>),
                                                   (^.))
import           Control.Monad                    (void, (<=<))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Logger             (NoLoggingT,
                                                   runStderrLoggingT)
import           Control.Monad.Reader             (ReaderT)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BSC
import           Data.Char                        (toLower)
import           Data.Foldable                    (traverse_)
import           Data.String                      (IsString)
import qualified Database.Esqueleto.Experimental  as Esqueleto
import           Database.Persist                 (PersistEntity)
import           Database.Persist.Postgresql      (SqlBackend,
                                                   withPostgresqlPool)
import           Database.Persist.Sql             (runMigration,
                                                   runSqlPersistMPool)
import           DB.Generators.BuyHistory         (fillShopHistory)
import           DB.Generators.BuyHistoryDetails  (fillShopHistoryDetails)
import           DB.Generators.Customer           (fillCustomer)
import           DB.Generators.PriceHistory       (fillPriceHistory)
import           DB.Generators.Product            (fillProduct)
import           DB.Generators.Wishlisted         (fillWishlisted)
import qualified Hedgehog.Range                   as Range
import           System.Environment               (getArgs)
import           Tables                           (Customer, Product,
                                                   ProductPriceHistory (ProductPriceHistory),
                                                   ShopHistory,
                                                   ShopHistoryDetails,
                                                   Wishlisted (Wishlisted),
                                                   migrateAll)


-- | Diverse testing parameters ends up here.
data TestingOptions = TOpt
    { _totalCustomers         :: Range.Range Int -- ^ Range of possible customers in the testing DB
    , _totalProducts          :: Range.Range Int -- ^ Range of possible products in the testing DB
    , _totalShopCustomer      :: Range.Range Int -- ^ Range of shoppings made by a customer in the testing DB
    , _totalProductCustomer   :: Range.Range Int -- ^ Range of possible products bought in the testing DB
    , _totalPriceHistory      :: Range.Range Int -- ^ How many price changes have there been
    , _totalWishlistedRepeats :: Range.Range Int -- ^ How many items each user have wishlisted
    }

makeLenses ''TestingOptions

-- | Where are the .env configuration files.
dbEnvConfig :: Config
dbEnvConfig = Config [".env"] [".env.example"] False

-- | Mapping of PostgreSQL [options](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING)
-- to the environment variables defined in .env file.
dbOptionToEnvVar :: (IsString a,IsString b) => [(a,b)]
dbOptionToEnvVar =
    [ ("host", "DEV_DB_HOST")
    , ("port", "DEV_DB_PORT")
    , ("user", "DEV_DB_USER")
    , ("password", "DEV_DB_PASS")
    , ("dbname", "DEV_DB_NAME")
    ]

-- | Build the testing options from the environment variables
mlTestingOptions :: IO TestingOptions
mlTestingOptions =
    ( ("DEV_DB_TOTAL_CUSTOMERS_LOW","DEV_DB_TOTAL_CUSTOMERS_HIGH")
    , ("DEV_DB_TOTAL_PRODUCTS_LOW","DEV_DB_TOTAL_PRODUCTS_HIGH")
    , ("DEV_DB_TOTAL_SHOPPING_CUSTOMER_LOW","DEV_DB_TOTAL_SHOPPING_CUSTOMER_HIGH")
    , ("DEV_DB_TOTAL_PRODUCTS_CUSTOMER_LOW","DEV_DB_TOTAL_PRODUCTS_CUSTOMER_HIGH")
    , ("DEV_DB_TOTAL_PRODUCTS_CHANGE_LOW","DEV_DB_TOTAL_PRODUCTS_CHANGE_HIGH")
    , ("DEV_DB_TOTAL_WISHLISTED_ITEMS_LOW","DEV_DB_TOTAL_WISHLISTED_ITEMS_HIGH")
    )
    &   traverseOf (each . each)  lookupEnv'
    <&> uncurry' TOpt . over each (uncurry Range.constant)
    where
        uncurry' f (_1,_2,_3,_4,_5,_6) = f _1 _2 _3 _4 _5 _6
        lookupEnv' s = maybe (ioError' $ "Could not find Environment Variable: " <> s) (pure . read @Int) <=< lookupEnv $ s
        ioError'   = ioError . userError

-- | Constructs a connection string from the environment variables defined in 'dbOptionToEnvVar'.
getConnectionString :: IO ByteString
getConnectionString
    = fmap (BSC.unwords . fmap (\ (a, b) -> a <> "=" <> b))
    . (traverse . traverse)  lookupEnv'
    $ dbOptionToEnvVar
    where
        lookupEnv' s = maybe (ioError' $ "Could not find Environment Variable: " <> s) (pure . BSC.pack) <=< lookupEnv $ s
        ioError'   = ioError . userError

-- | Deletes all the rows from all the tables of the DB.
flushDB :: (MonadIO m,MonadResource m) => ReaderT SqlBackend m ()
flushDB = do
    deleteTableRows @Wishlisted
    deleteTableRows @ProductPriceHistory
    deleteTableRows @ShopHistoryDetails
    deleteTableRows @ShopHistory
    deleteTableRows @Product
    deleteTableRows @Customer
    where
        deleteTableRows :: forall entity m'. (MonadIO m',PersistEntity entity) => ReaderT SqlBackend m' ()
        deleteTableRows = Esqueleto.delete $ void $ Esqueleto.from $ Esqueleto.table @entity


-- | Populates the DB with random data.
populateDB :: (MonadIO m,MonadResource m) => TestingOptions -> ReaderT SqlBackend m ()
populateDB to = do
    -- run the migrations against the db
    runMigration migrateAll

    -- fill the customer table
    to ^. totalCustomers & fillCustomer
    -- fill the product table 
    to ^. totalProducts  & fillProduct
    -- fill the history table
    to ^. totalShopCustomer & fillShopHistory
    -- fill the history details table
    to ^. totalProductCustomer & fillShopHistoryDetails
    -- fill the price history details table
    to ^. totalPriceHistory & fillPriceHistory
    -- fill the wishlisted table
    to ^. totalWishlistedRepeats & fillWishlisted


pArgs :: (MonadIO m, MonadResource m) => TestingOptions -> ReaderT SqlBackend m ()
pArgs to = liftIO ((fmap . fmap) toLower <$> getArgs) >>= \args -> do
    case args of
        ["populate"] -> flushDB >> populateDB to
        ["flush"]    -> flushDB
        _            -> pure ()


exec :: IO ()
exec
    = loadFile dbEnvConfig
    >> getConnectionString
    >>= \connectionString -> mlTestingOptions
    >>= \testingOptions   -> runStderrLoggingT $ withPostgresqlPool connectionString 4
    $   \pool -> liftIO $ runSqlPersistMPool (pArgs testingOptions) pool




    