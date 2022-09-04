{-|
Module      : Tables
Description : Defines the DB tables and migrations
Copyright   : (c) Daniel Pinto, 2022
License     : BSD-3
Maintainer  : Daniel.Andres.Pinto@gmail.com
Stability   : experimental
Portability : POSIX

In order to consult further about the domains, please check "Domains.Domains" file.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Tables where


import           Control.Monad       (unless)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time.Clock     (UTCTime)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Domains.Domains     (NonEmptyText, NonNegInt, NonNegPrice,
                                      PositiveInt, UsernameText)

-------------------------------------





share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- | Holds bare minimum information for a customer.
Customer sql=customer
    -- | Every customer has a unique username that serves as it PK
    username    UsernameText sql=username
    -- | Password should be saved hashed. Never in plaintext
    uPassword   NonEmptyText sql=u_password
    -- | emails are always unique.
    email       NonEmptyText sql=email  

    UniqueUsername  username
    UniqueEmail     email
    Primary         username
    deriving Show Read

-- | Holds bare minimum information for products
Product sql=product
    -- | Every product has a unique ID that serves as its PK.
    Id                  Int             sql=product_id
    -- | Name of the product, names does not need to be unique (thus cannot be PK).
    productName         NonEmptyText    sql=product_name
    -- | Every product should have a non-empty description.
    productDescription  NonEmptyText    sql=product_description
    -- | How many of the product are in stock.
    productAvaliability NonNegInt       sql=product_avaliability

    deriving Show Read

-- | The shopping history of a customer
ShopHistory sql=buy_history
    -- | Every transaction has a unique ID that serves as its PK
    Id          Int                         sql=buy_id
    -- | Every transaction is made by a customer
    username    CustomerId                  sql=username
    -- | Timestamp of the transaction
    buyDate     UTCTime                     sql=buy_date
    -- | Any miscelanea details are expressed here
    miscDetails NonEmptyText    Maybe       sql=details

    deriving Show Read

-- | Concrete details about shopping history, allows to reference the 
ShopHistoryDetails sql=buy_history_details
    -- | The transaction ID
    buyID       ShopHistoryId   sql=buy_id
    -- | The product ID
    productID   ProductId       sql=product_id
    -- | Quantity sold
    quantity    PositiveInt     sql=quantity

    Primary buyID productID 

    deriving Show Read

-- | Price History for each product.
ProductPriceHistory sql=product_price_history
    -- | The ID of the product
    productID   ProductId   sql=product_id
    -- | The date of the recorded price
    priceDate   UTCTime     sql=price_date
    -- | The price recorded
    price       NonNegPrice sql=price_us_dollar

    Primary productID priceDate

    deriving Show Read

-- | Wishlist for every customer
Wishlisted sql=wishlisted
    -- | Owner of the wishlisted item.
    username        CustomerId  sql=username
    -- | Wishlisted product.
    productID       ProductId   sql=product_id
    -- | Desired quantity of the product
    desiredQuantity PositiveInt sql=desired_quantity

    Primary username productID

    deriving Show Read
|]


