{-|
Module      : Domains.Internal.DomainsTH
Description : Holds additional instances for the mappings of the DB Domains to Haskell types.
Copyright   : (c) Daniel Pinto, 2022
License     : BSD-3
Maintainer  : Daniel.Andres.Pinto@gmail.com
Stability   : experimental
Portability : POSIX

Holds necessary instances to work with the Domains defined in 
"Domains.Internal.Domains".
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Domains.Internal.DomainsTH where

import           Domains.Internal.Domains (CurrentScale', Eval (..),
                                           NonEmptyText (UnsafeNET),
                                           NonNegInt (UnsafeNNI),
                                           NonNegPrice (UnsafeNNP),
                                           PositiveInt (UnsafePI), Tag (..),
                                           UsernameText (UnsafeUT),
                                           makeFromHttpApiDataRestricted,
                                           makeFromJsonRestricted,
                                           makePathPieceRestricted,
                                           makePersistFieldRestricted, mkX)

import           Control.Lens             (_1, over)
import           Control.Monad            (unless, (<=<))
import           Control.Monad.Except     (MonadError (throwError))
import           Data.Aeson               (FromJSON (..), ToJSON)
import           Data.Aeson.Types         (parseFail)
import           Data.Coerce              (Coercible, coerce)
import           Data.Foldable            (traverse_)
import           Data.Serialize           (Serialize)
import           Data.Serialize.Text      ()
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Typeable            (Proxy (Proxy), Typeable, typeRep)
import           Database.Persist.Sql     (PersistField (..),
                                           PersistFieldSql (..),
                                           PersistValue (..), SqlType (..))
import           GHC.Generics             (Generic)
import           Language.Haskell.TH      (Dec, Q, conT, mkName)
import           Money                    (Approximation (HalfEven))
import qualified Money
import           Money.Cereal             ()
import           Web.HttpApiData          (FromHttpApiData (..), ToHttpApiData)
import           Web.PathPieces           (PathPiece (..), readFromPathPiece,
                                           showToPathPiece)


----------------------------------------
-- Instances
----------------------------------------

instance Serialize NonNegPrice

instance PersistField NonNegPrice where
    toPersistValue  = PersistRational . toRational  . eval @Accesor
    fromPersistValue pv = case pv of
        PersistInt64 in' ->
            let v = coerce . Money.discrete @CurrentScale' . fromIntegral $ in'
            in mkX (eval @Condition @NonNegPrice v) errMSG v
        PersistRational ra -> do 
            (v,rem) <- maybe (Left errMSG) Right
                $ ( 
                    ( pure 
                    . over _1 coerce 
                    . Money.discreteFromDense @_ @CurrentScale' HalfEven
                    ) 
                    <=< Money.dense
                  ) ra
            unless (rem ==  0) (Left . remainderErrMSG $ ra)
            mkX (eval @Condition @NonNegPrice v) errMSG v
        _ -> Left . typeErrMSG $ pv
        where
            errMSG     = "Value has the right type but doesn't pass the Domains check."
            remainderErrMSG n
                = "Non zero money remainder when doing convertion to Scale (\"USD\",\"cents\").\
                \ Please input a rational number that can be precisely translated\n\
                \ Given number: " <> (Text.pack . show ) n
            typeErrMSG x = "Cannot parse values of type: " <> (Text.pack . head . words . show $ x)

instance PersistFieldSql NonNegPrice where
    sqlType _ = SqlOther "nonnegprice"


$(makePersistFieldRestricted @PositiveInt @Int)

instance PersistFieldSql PositiveInt where
    sqlType _ = SqlOther "positiveint"

$(makePersistFieldRestricted @NonNegInt @Int)

instance PersistFieldSql NonNegInt where
    sqlType _ = SqlOther "nonnegint"

$(makePersistFieldRestricted @NonEmptyText @Text)

instance PersistFieldSql NonEmptyText where
    sqlType _ = SqlOther "nonemptytext"

$(makePersistFieldRestricted @UsernameText @Text)

instance PersistFieldSql UsernameText where
    sqlType _ = SqlOther "usernametext"

$(makeFromJsonRestricted @PositiveInt   @Int )
$(makeFromJsonRestricted @NonNegInt     @Int )
$(makeFromJsonRestricted @NonEmptyText  @Text)
$(makeFromJsonRestricted @UsernameText  @Text)

$(makePathPieceRestricted @PositiveInt   @Int )
$(makePathPieceRestricted @NonNegInt     @Int )
$(makePathPieceRestricted @NonEmptyText  @Text)
$(makePathPieceRestricted @UsernameText  @Text)

$(makeFromHttpApiDataRestricted @PositiveInt   @Int )
$(makeFromHttpApiDataRestricted @NonNegInt     @Int )
$(makeFromHttpApiDataRestricted @NonEmptyText  @Text)
$(makeFromHttpApiDataRestricted @UsernameText  @Text)