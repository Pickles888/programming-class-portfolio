module CSS where

data CSSObject = CSSObject String [(String, String)]

mkDef :: String -> String -> (String, String)
mkDef a b = (a, b)

type CSS = [CSSObject]

instance Show CSSObject where
  show (CSSObject name xs) = name ++ "{" ++ concatMap (\(a, b) -> a ++ ": " ++ b ++ ";") xs ++ "}"

mkVar :: String -> Maybe String -> String
mkVar var val = var ++ maybe "" (("=" ++) . show) val