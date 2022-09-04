{-|
Module      : Domains.Domains
Description : Re exports some of the internal definitions of the Domains.Internal modules.
Copyright   : (c) Daniel Pinto, 2022
License     : BSD-3
Maintainer  : Daniel.Andres.Pinto@gmail.com
Stability   : experimental
Portability : POSIX

Every module that needs to work with Domains must import **this** module for safety.
Internal modules are exposed for mere illustration.
-}
module Domains.Domains 
    ( DI.NonNegPrice
    , DI.mkNNP
    , DI.getPrice
    , DI.PositiveInt
    , DI.mkPI
    , DI.getPositiveInt
    , DI.NonNegInt
    , DI.mkNNI
    , DI.getNonNegInt
    , DI.UsernameText
    , DI.mkUT
    , DI.getUsernameText
    , DI.NonEmptyText
    , DI.mkNET
    , DI.getNonEmptyText
    , DI.CurrentCurrency
    , DI.CurrentScale
    , DI.CurrentScale'
    , DI.currentScale
    , module DI.TH
    ) where

import qualified Domains.Internal.Domains as DI
import qualified Domains.Internal.DomainsTH as DI.TH

