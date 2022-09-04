{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module DB.Generators.BuyHistory where

import           Conduit                     (ConduitM, MonadIO, MonadResource,
                                              ZipSource (ZipSource, getZipSource),
                                              concatMapC, mapM_C, runConduit,
                                              (.|))
import qualified Conduit
import           Control.Monad.Except        (MonadIO, void)
import           Control.Monad.Reader        (MonadReader,
                                              ReaderT (ReaderT, runReaderT))
import           Data.Conduit                (ConduitM,
                                              ZipSource (ZipSource, getZipSource),
                                              (.|))
import           Data.Conduit.Combinators    (repeatM)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time                   (UTCTime (UTCTime))
import           Database.Persist            (PersistEntity (Key),
                                              PersistStoreWrite (insert),
                                              selectKeys)
import           Database.Persist.Postgresql (SqlBackend)
import           DB.Generators.Domains       (genDate, genNET)
import           Domains.Domains             (NonEmptyText)
import           Hedgehog                    (Gen)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Tables                      (Customer, Key,
                                              ShopHistory (ShopHistory))





-- | Randomly generates buy details (either a 'NonEmptyText' in case details were provided or nothing otherwise).
genBuyDetails :: Gen (Maybe NonEmptyText)
genBuyDetails = Gen.choice [Just <$> genNET,pure Nothing]

-- | Randomly generates a shopping history for each customer. 
generateShopHistory
    :: (MonadResource m, MonadReader SqlBackend m, MonadIO m)
    => Range.Range Int
    -> ConduitM () ShopHistory m ()
generateShopHistory range
    = getZipSource
    $ ShopHistory
    <$> ZipSource genCustomerIDs
    <*> ZipSource genBuyDate'
    <*> ZipSource genBuyDetails'
    where
        genBuyDetails'  :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Maybe NonEmptyText) m ()
        genBuyDetails'  = repeatM $ Gen.sample genBuyDetails

        genBuyDate' :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () UTCTime m ()
        genBuyDate' = repeatM $ Gen.sample genDate

        paired :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Int,Key Customer) m ()
        paired
            = getZipSource
            $ (,)
            <$> ZipSource (repeatM $ Gen.sample (Gen.integral range)  )
            <*> ZipSource (selectKeys [] [])


        genCustomerIDs :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Key Customer) m ()
        genCustomerIDs = paired .| Conduit.concatMapC (uncurry replicate)

-- | Fills the shop history table randomly.
fillShopHistory :: (MonadResource m, MonadIO m) => Range.Range Int -> ReaderT SqlBackend m ()
fillShopHistory range
    = Conduit.runConduit
    $ generateShopHistory range
    .| Conduit.mapM_C (void . insert)
