module Utils where
import Data.Text              (Text, pack)
import Text.Pandoc.Definition (Block (..), Inline (..), MetaValue (..))

mvToBlocks :: MetaValue -> [Block]
mvToBlocks (MetaMap _)       = []
mvToBlocks (MetaList v)      = concatMap mvToBlocks v
mvToBlocks (MetaBool b)      = [ Plain  [ Str $ show b ] ]
mvToBlocks (MetaString s)    = [ Plain [ Str s ] ]
mvToBlocks (MetaInlines ins) = [Plain ins]
mvToBlocks (MetaBlocks bs)   = bs

fromRight :: Either a b -> b
fromRight ~(Right a) = a

tshow :: Show a => a -> Text
tshow = pack . show

readMaybe :: Read a => String -> Maybe a
readMaybe str =
  case reads str of
    [(a, "")] -> Just a
    _         -> Nothing
