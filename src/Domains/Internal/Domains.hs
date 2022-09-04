{-|
Module      : Domains.Internal.Domains
Description : Holds the mappings of the DB Domains to Haskell types.
Copyright   : (c) Daniel Pinto, 2022
License     : BSD-3
Maintainer  : Daniel.Andres.Pinto@gmail.com
Stability   : experimental
Portability : POSIX

Holds the mappings of the DB Domains to Haskell types, additional instances
are exposed in the "Domains.Internal.DomainsTH" due to stage restrictions.
-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}



module Domains.Internal.Domains where


import           Control.Monad        (unless, (<=<), (>=>))
import           Control.Monad.Except (MonadError (throwError),
                                       MonadPlus (mzero))
import           Data.Aeson           (FromJSON (..), ToJSON)
import           Data.Aeson.Types     (parseFail)
import           Data.Coerce          (Coercible, coerce)
import           Data.Serialize       (Serialize)
import           Data.Serialize.Text  ()
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Typeable        (Proxy (Proxy), Typeable, typeRep)
import           Database.Persist.Sql (PersistField (..), PersistFieldSql (..),
                                       PersistValue (..), SqlType (..))
import           GHC.Generics         (Generic)
import           GHC.TypeLits         (natVal)
import           Language.Haskell.TH  (Dec, Q, conT, mkName)
import qualified Money
import           Web.HttpApiData      (FromHttpApiData (..), ToHttpApiData)
import           Web.PathPieces       (PathPiece (..), readFromPathPiece,
                                       showToPathPiece)

----------------------------------------
-- Types and constructors
----------------------------------------

-- | Universal currency. TODO: think if this should belong to a config file.
type CurrentCurrency = "USD"
-- | Universal scale.  TODO: think if this should belong to a config file.
type CurrentScale    = "cent"
-- | Universal UnitScale. 
type CurrentScale'   = Money.UnitScale CurrentCurrency CurrentScale

-- | Universal UnitScale as a term.
currentScale :: Num a => (a, a)
currentScale = (low,high)
    where
        low  = fromIntegral . natVal $ Proxy @(Fst CurrentScale')
        high = fromIntegral . natVal $ Proxy @(Snd CurrentScale')


-- | Represents a non negative price.
newtype NonNegPrice = UnsafeNNP {getPrice :: Money.Discrete CurrentCurrency CurrentScale }
    deriving stock (Show,Read,Generic)
    deriving newtype (Eq,Ord)


-- | Smart constructor around NonNegPrice wrap.
mkNNP
    :: MonadError Text m
    => Money.Discrete CurrentCurrency CurrentScale  -- ^ Desired Money to wrap
    -> m NonNegPrice                                -- ^ Either an error if the money is negative, or the wrap.
mkNNP entry
    = mkX condition errorMSG entry
    where
        condition = condition' @NonNegPrice entry
        errorMSG = "Entry error: " <> (Text.pack . show) entry <> " Should be a non negative number"

-- | Represents a positive integeger
newtype PositiveInt = UnsafePI {getPositiveInt :: Int}
    deriving stock (Show,Read,Generic)
    deriving newtype (Eq,Ord,ToJSON,Serialize,Typeable,ToHttpApiData)


-- | Smart constructor around PositiveInt wrap.
mkPI
    :: MonadError Text m
    => Int                  -- ^ Desired Int to wrap
    -> m PositiveInt        -- ^ Either an error if the integer is not positive, or the wrap.
mkPI entry
    = mkX condition errorMSG entry
    where
        condition = condition' @PositiveInt entry
        errorMSG  = "Entry error: " <> (Text.pack . show) entry <> " Should be positive"

-- | Represents a positive integeger
newtype NonNegInt = UnsafeNNI {getNonNegInt :: Int}
    deriving stock (Show,Read,Generic)
    deriving newtype (Eq,Ord,ToJSON,Serialize,Typeable,ToHttpApiData)

-- | Smart constructor around NonNegInt wrap.
mkNNI
    :: MonadError Text m
    => Int                  -- ^ Desired Int to wrap
    -> m NonNegInt        -- ^ Either an error if the integer is negative, or the wrap.
mkNNI entry
    = mkX condition errorMSG entry
    where
        condition = condition' @NonNegInt entry
        errorMSG  = "Entry error: " <> (Text.pack . show) entry <> " Should be non negative"

-- | Represents a valid username.
newtype UsernameText = UnsafeUT {getUsernameText :: Text}
    deriving stock (Show,Read,Generic)
    deriving newtype (Eq,Ord,ToJSON,Serialize,Typeable,ToHttpApiData)

-- | Smart constructor around UsernameText wrap.
mkUT
    :: MonadError Text m
    => Text                 -- ^ Desired username to wrap
    -> m UsernameText       -- ^ Either an error if the text is not a valid username, or the wrap.
mkUT entry
    =  mkX condition errorMSG entry
    where
        condition = condition' @UsernameText entry
        errorMSG  = "Entry error: " <> entry <> " Should have between 8 and 20 non space characters"

-- | Represents a non empty text (text that is all whitespace is counted as empty)
newtype NonEmptyText = UnsafeNET {getNonEmptyText :: Text}
    deriving stock (Show,Read,Generic)
    deriving newtype (Eq,Ord,ToJSON,Serialize,Typeable,ToHttpApiData,Semigroup,Monoid)

-- | Smart constructor around NonEmptyText wrap
mkNET
    :: MonadError Text m
    => Text                 -- ^ Desired text to wrap
    -> m NonEmptyText       -- ^ Either an error if the text is empty/just whitespaces, or the wrap.
mkNET entry
    =  mkX condition errorMSG entry
    where
        condition = condition' @NonEmptyText entry
        errorMSG  = "Entry error: " <> entry <> " Should have between 8 and 20 non space characters"


----------------------------------------
-- Utilities
----------------------------------------

type family Fst (t :: (a,b)) :: a where
    Fst '(a,_) = a

type family Snd (t :: (a,b)) :: b where
    Fst '(_,b) = b


-- | Tag serves as analog for function names. Allows us to have multiple Eval instances for the same base type
-- but different purposes (one for accessing data, one for checking conditions...)
data Tag = Accesor | Condition

-- | Template Haskell doesnt allow to use function parameters inside of its quoters (they don't have a Lift instance).
-- Therefore we use defunctionalization to promote functions to constraints which are known at compile time.
class Eval (tag :: Tag) l t | tag l -> t where
    eval ::  l -> t


-- | Generates PersistField instances for Restricted Domains.
makePersistFieldRestricted
    :: forall a b.
        ( Typeable b
        , Typeable a
        , PersistField b
        , Coercible b a
        , Eval Accesor a b
        , Eval Condition a Bool)
    => Q [Dec]
makePersistFieldRestricted = [d|
    instance PersistField $t where
        toPersistValue      = toPersistValue . eval @Accesor @($t) @($t')
        fromPersistValue    = checker  <=< fromPersistValue @($t')
            where
                combinator x = unless ((eval @Condition) x ) (Left errMSG) >> pure x
                checker = combinator  . coerce
    |]
    where
        t  = conT . mkName . show . typeRep $ Proxy @a
        t' = conT . mkName . show . typeRep $ Proxy @b
        errMSG :: Text
        errMSG = "Value has the right type but doesn't pass the Domains check."

-- | Generates FromJSON instances for Restricted Domains.
makeFromJsonRestricted
    :: forall a b.
        ( Typeable b
        , Typeable a
        , FromJSON b
        , Coercible b a
        , Eval Condition a Bool
        )
    => Q [Dec]
makeFromJsonRestricted = [d|
    instance FromJSON $t where
        parseJSON value = parseJSON @($t') value >>= \value
            -> unless (condition' @($t) value) (parseFail errMSG)
            >> pCoerce value
        parseJSONList value = parseJSONList @($t') value >>= \values
            -> traverse_ (\v -> unless (condition' @($t) v) (parseFail errMSG)) values
            >> traverse pCoerce values
    |]
    where
        t  = conT . mkName . show . typeRep $ Proxy @a
        t' = conT . mkName . show . typeRep $ Proxy @b

        errMSG :: [Char]
        errMSG = "Value has the right type but doesn't pass the Domains check."

-- | Generates PathPiece instances for Restricted Domains.
makePathPieceRestricted
    :: forall a b.
        ( Typeable b
        , Typeable a
        , FromJSON b
        , Coercible b a
        , Eval Condition a Bool
        , Eval Accesor a b
        )
    => Q [Dec]
makePathPieceRestricted = [d|
    instance PathPiece $t where
        fromPathPiece v = fromPathPiece @($t') v >>= \v
            -> unless (condition' @($t) v) mzero
            >> pCoerce v
        toPathPiece = toPathPiece @($t') . eval @Accesor
    |]
    where
        t  = conT . mkName . show . typeRep $ Proxy @a
        t' = conT . mkName . show . typeRep $ Proxy @b

        errMSG :: [Char]
        errMSG = "Value has the right type but doesn't pass the Domains check."


-- | Generates FromHttpApiData sinstances for Restricted Domains.
makeFromHttpApiDataRestricted
    :: forall a b.
        ( Typeable b
        , Typeable a
        , FromJSON b
        , Coercible b a
        , Eval Condition a Bool
        )
    => Q [Dec]
makeFromHttpApiDataRestricted = [d|
    instance FromHttpApiData $t where
        parseUrlPiece
            = parseUrlPiece @($t')
            >=> \v -> mkX (condition' @($t) v) errMSG v
        parseQueryParam
            = parseQueryParam @($t')
            >=> \v -> mkX (condition' @($t) v) errMSG v
    |]
    where
        t  = conT . mkName . show . typeRep $ Proxy @a
        t' = conT . mkName . show . typeRep $ Proxy @b

        errMSG :: [Char]
        errMSG = "Value has the right type but doesn't pass the Domains check."

-- | Given the condition for a domain, an error message and a value to coerce
-- try to coerce the value yielding the error message in case the condition does _not_ hold.
mkX :: (Coercible a b, MonadError Text m) => Bool -> Text -> a -> m b
mkX condition errMSG x
    = unless condition (throwError errMSG)
    >> pCoerce x


-- | Coerce and embed a value into an applicative context.
pCoerce :: (Coercible a b, Applicative f) => a -> f b
pCoerce = pure . coerce

-- | Useful for smart constructors.
condition' :: forall a b. (Eval Condition a Bool, Coercible b a) => b -> Bool
condition' = eval @Condition @a . coerce



----------------------------------------
-- Instances for eval
----------------------------------------

instance Eval Accesor PositiveInt Int where
    eval = getPositiveInt

instance Eval Condition PositiveInt Bool where
    eval = (> 0) . eval @Accesor

instance Eval Accesor NonNegInt Int where
    eval = getNonNegInt

instance Eval Condition NonNegInt Bool where
    eval = (>= 0) . eval @Accesor

instance Eval Accesor NonEmptyText Text where
    eval = getNonEmptyText

instance Eval Condition NonEmptyText Bool where
    eval = notNull . eval @Accesor
        where
            isWhiteSpace = (||) <$> (== ' ') <*> (== '\t')
            notNull      = not . Text.null . Text.dropWhile isWhiteSpace

instance Eval Accesor UsernameText Text where
    eval = getUsernameText

instance Eval Condition UsernameText Bool where
    eval = condition . eval @Accesor
        where
            isWhiteSpace     = (||) <$> (== ' ') <*> (== '\t')
            between low high = (&&) <$> (>= low) <*> (<= high)
            correctLength = between 8 20 . Text.length
            condition     = (&&) <$> correctLength <*> Text.all (not . isWhiteSpace)


instance (scale ~ CurrentScale') => Eval Accesor NonNegPrice (Money.Discrete' CurrentCurrency scale) where
  eval = getPrice

instance Eval Condition NonNegPrice Bool where
    eval = condition . getPrice
        where
            condition = (>= Money.discrete 0)