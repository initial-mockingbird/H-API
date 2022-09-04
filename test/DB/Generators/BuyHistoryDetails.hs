{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module DB.Generators.BuyHistoryDetails where
import           Conduit                     (MonadResource, Void)
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
import           DB.Generators.Utils         (eitherToMaybe)
import           Domains.Domains             (mkPI)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Tables                      (ShopHistoryDetails (ShopHistoryDetails))



-- | Fills the shop history details table randomly.
fillShopHistoryDetails :: (MonadResource m, MonadIO m) => Range.Range Int -> ReaderT SqlBackend m ()
fillShopHistoryDetails range
    = Conduit.runConduit
    $ genSHD
    .| Conduit.mapM_C (void . insert)
    where
        genSHD :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () ShopHistoryDetails m ()
        genSHD
            = getZipSource
            $ ShopHistoryDetails
            <$> ZipSource (selectKeys [] [])
            <*> ZipSource (selectKeys [] [])
            <*> ZipSource (repeatM $ Gen.sample $ Gen.just $ eitherToMaybe . mkPI <$> Gen.integral range)
