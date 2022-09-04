module DB.Generators.Utils
    ( eitherToMaybe
    ) where

-- | Natural transformation from Either to Maybe.
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe _         = Nothing 