{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module DB.Generators.Customer where

import           Conduit                     (ConduitT, MonadIO, MonadResource,
                                              mapM_C, runConduit, takeC, (.|))
import           Control.Monad.Except        (MonadIO, void)
import           Control.Monad.Reader        (ReaderT)
import           Data.Conduit.Combinators    (repeatM)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Database.Persist            (PersistStoreWrite (insert))
import           Database.Persist.Postgresql (SqlBackend)
import           DB.Generators.Domains       (genNET, genUT)
import           DB.Generators.Utils         (eitherToMaybe)
import           Domains.Domains             (NonEmptyText, mkNET)
import           Hedgehog                    (Gen, Range)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Tables                      (Customer (Customer))


-- | Generates a "random" email (sadly no RFC complaint)
emailGen :: Gen NonEmptyText
emailGen = Gen.just $ eitherToMaybe . mkNET <$> do
    name      <- validName
    domain    <- validDomain
    extension <- validExtension
    let email = name <> "@" <> domain <> "." <> extension
    pure email 
    where 
        range = Range.constantFrom 8 8 20

        validName :: Gen Text
        validName      
            = Gen.text range 
            $ Gen.frequency [(3,Gen.alphaNum),(1,Gen.element specialChar)]

        validDomain :: Gen Text
        validDomain    = Gen.text (Range.constantFrom 3 3 8) Gen.alphaNum

        validExtension :: Gen Text
        validExtension = Gen.element ["com","es","org"]

        specialChar :: [Char]
        specialChar    = "_*-+/*&^$#!=~?/,.|"

-- | Estimates a range of users to be inserted. Used in 'genTotalUsers'.
totalUsersBounds :: Range Int
totalUsersBounds = Range.constant 15 1000

-- | A generator for the total number of users that the system is going to have.
genTotalUsers :: Gen Int
genTotalUsers = Gen.integral totalUsersBounds

-- | Ramdomly generates a non-ending stream of customers
customerSource ::  MonadIO m => ConduitT () Customer m ()
customerSource = repeatM $ do
        username  <- Gen.sample genUT
        password  <- Gen.sample genNET
        email     <- Gen.sample emailGen
        pure $ Customer username password email

-- | Fills the shop history table randomly.
fillCustomer :: (MonadResource m, MonadIO m) => Range.Range Int -> ReaderT SqlBackend m ()
fillCustomer range 
    = Conduit.runConduit
    $ customerSource
    .| (Gen.sample  (Gen.integral range) >>= takeC )
    .| Conduit.mapM_C (void . insert)


