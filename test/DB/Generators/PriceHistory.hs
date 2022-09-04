{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module DB.Generators.PriceHistory where

import           Conduit                     (MonadResource, Void, takeC)
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
import           Database.Persist            (PersistStoreWrite (insert),
                                              selectKeys)
import           Database.Persist.Postgresql (SqlBackend)
import           DB.Generators.Domains       (genDate, genNNP)
import           DB.Generators.Utils         (eitherToMaybe)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Tables                      (Key, Product,
                                              ProductPriceHistory (ProductPriceHistory))




-- | Fills the shop history details table randomly.
fillPriceHistory :: (MonadResource m, MonadIO m) => Range.Range Int -> ReaderT SqlBackend m ()
fillPriceHistory range
    = Conduit.runConduit
    $ genPH 
    .| Conduit.mapM_C (void . insert)
    where
        paired :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Int,Key Product) m ()
        paired
            = getZipSource
            $ (,)
            <$> ZipSource (repeatM $ Gen.sample (Gen.integral range)  )
            <*> ZipSource (selectKeys [] [])


        genProductIDs :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Key Product) m ()
        genProductIDs = paired .| Conduit.concatMapC (uncurry replicate)

        genPH :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () ProductPriceHistory m ()
        genPH 
            = getZipSource 
            $ ProductPriceHistory
            <$> ZipSource genProductIDs
            <*> ZipSource (repeatM $ Gen.sample genDate)
            <*> ZipSource (repeatM $ Gen.sample genNNP) 
