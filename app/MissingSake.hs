{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction, TypeApplications #-}
module MissingSake where
import GHC.Generics
import Web.Sake

setItemBody :: a1 -> Item a2 -> Item a1
setItemBody bdy i = i { itemBody = bdy }

tryWithFile :: MonadAction m => FilePath -> m a -> m (Maybe a)
tryWithFile fp act = do
  ex <- liftAction $ doesFileExist fp
  if ex
    then Just <$> act
    else return Nothing
