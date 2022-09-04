{-# LANGUAGE TypeApplications #-}
module DB.Generators.Domains
    ( genNNP
    , genNNI
    , genPI
    , genUT
    , genNET
    , genDate
    ) where



import           Data.Int            (Int32)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time           (UTCTime (UTCTime))
import           DB.Generators.Utils (eitherToMaybe)
import           Domains.Domains     (NonEmptyText, NonNegInt, NonNegPrice,
                                      PositiveInt, UsernameText, currentScale,
                                      mkNET, mkNNI, mkNNP, mkPI, mkUT)
import           Hedgehog            (Gen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Money



-- | Generates a random 'NonNegPrice'.
genNNP :: Gen NonNegPrice
genNNP = generator
    where
        (low,high) = currentScale
        high'      = lcm high 1000
        generator  = Gen.just $ eitherToMaybe . mkNNP . Money.discrete <$> Gen.integral (Range.constant 1 (high'*low))

-- | Generates a random 'NonNegInt'.
genNNI :: Gen NonNegInt
genNNI = generator 
    where
        generator = Gen.just $ eitherToMaybe . mkNNI <$> Gen.integral (Range.constant 0 (fromIntegral $ maxBound @Int32))

-- | Generates a random 'PositiveInt'.
genPI :: Gen PositiveInt
genPI = generator 
    where
        generator = Gen.just $ eitherToMaybe . mkPI <$> Gen.integral (Range.constant 1 (fromIntegral $ maxBound @Int32))

-- | Generates a random 'UsernameText'.
genUT :: Gen UsernameText
genUT = generator
    where
        generator = Gen.just $  eitherToMaybe . mkUT <$> Gen.text (Range.constant 8 20) genChar

        genChar = Gen.frequency [(3,Gen.alphaNum),(1,Gen.element ".~!@#$%^&*()_+=-{}?><),./;'[]|")]

-- | Generates a random 'NonEmptyText'.
genNET :: Gen NonEmptyText
genNET = generator
    where
        generator = Gen.just $  eitherToMaybe . mkNET <$> Gen.text (Range.constant 20 40) genChar

        genChar = Gen.frequency [(3,Gen.alphaNum),(1,Gen.element ".~!@#$%^&*()_+=-{}?><),./;'[]|")]

-- | Ramdomly generates a Date from 1957 to 2022. TODO: should borders be parametrized?
genDate :: Gen UTCTime
genDate = do
    day  <- Gen.enum (toEnum 0) (toEnum years)
    time <- Gen.enum (toEnum 0) (toEnum 86400)
    pure $ UTCTime day time
    where
        years = (2022 - 1957) * 365
