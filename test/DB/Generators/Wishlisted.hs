{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module DB.Generators.Wishlisted where

import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Tables                      (Customer, Key,
                                              Wishlisted (Wishlisted))

import           Control.Monad.Except        (MonadIO, void)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time                   (UTCTime (UTCTime))
import           Database.Persist            (PersistStoreWrite (insert),
                                              selectKeys)

import           Conduit                     (MonadResource, Void, takeC)
import qualified Conduit
import           Control.Monad.Reader        (MonadReader,
                                              ReaderT (ReaderT, runReaderT))
import           Data.Conduit                (ConduitM,
                                              ZipSource (ZipSource, getZipSource),
                                              (.|))
import           Data.Conduit.Combinators    (repeatM)
import           Database.Persist.Postgresql (SqlBackend)
import           DB.Generators.Domains       (genPI)
import           DB.Generators.Utils         (eitherToMaybe)



-- | Fills the Wishlisted table randomly.
fillWishlisted :: (MonadResource m, MonadIO m) => Range.Range Int -> ReaderT SqlBackend m ()
fillWishlisted range
    = Conduit.runConduit
    $ genWL 
    .| Conduit.mapM_C (void . insert)
    where
        paired :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Int,Key Customer) m ()
        paired
            = getZipSource
            $ (,)
            <$> ZipSource (repeatM $ Gen.sample (Gen.integral range)  )
            <*> ZipSource (selectKeys [] [])

        genCustomerIDs :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () (Key Customer) m ()
        genCustomerIDs = paired .| Conduit.concatMapC (uncurry replicate)

        genWL :: (MonadResource m, MonadReader SqlBackend m, MonadIO m) => ConduitM () Wishlisted m ()
        genWL 
            = getZipSource 
            $ Wishlisted
            <$> ZipSource genCustomerIDs
            <*> ZipSource (selectKeys [] [])
            <*> ZipSource (repeatM $ Gen.sample genPI)
