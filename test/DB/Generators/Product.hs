module DB.Generators.Product where

import           Conduit                     (ConduitT, MonadResource, mapM_C,
                                              runConduit, takeC, (.|))
import           Control.Monad.Except        (MonadIO, void)
import           Control.Monad.Reader        (ReaderT)
import           Data.Conduit.Combinators    (repeatM)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Database.Persist            (PersistStoreWrite (insert))
import           Database.Persist.Postgresql (SqlBackend)
import           DB.Generators.Domains       (genNET, genNNI)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Tables                      (Product (Product))

-- | Ramdomly generates a non-ending stream of products
createProducts ::   MonadIO m => ConduitT () Product m () 
createProducts = repeatM $ do
    names        <- Gen.sample genNET        
    descriptions <- Gen.sample genNET
    stock        <- Gen.sample genNNI
    pure $ Product names descriptions stock

-- | Fills the product table randomly.
fillProduct :: (MonadResource m, MonadIO m) => Range.Range Int -> ReaderT SqlBackend m ()
fillProduct range 
    = Conduit.runConduit
    $ createProducts
    .| (Gen.sample  (Gen.integral range) >>= takeC )
    .| Conduit.mapM_C (void . insert)