module HTML where

import CSS
import Lib
import Control.Applicative (liftA2)
import GHC.Base ((<|>))

data HTMLProps = HTMLProps
  {
    cssClass :: Maybe String,
    extraVars :: [(String, Maybe String)]
  }

instance Semigroup HTMLProps where
  (HTMLProps {cssClass = c1, extraVars = v1}) <> (HTMLProps {cssClass = c2, extraVars =v2}) = 
    HTMLProps (liftA2 (\a b -> a ++ " " ++ b) c1 c2 <|> c1 <|> c2) (v1 ++ v2)

mkAttrs :: [(String, String)] -> HTMLProps
mkAttrs = HTMLProps Nothing . fmap (fmap Just)

mkAttr :: (String, String) -> HTMLProps
mkAttr (a, b) = HTMLProps Nothing [(a, Just b)]

mkFlags :: [String] -> HTMLProps
mkFlags = HTMLProps Nothing . map (, Nothing)

mkClass :: String -> HTMLProps
mkClass className = HTMLProps (Just className) []

mkHyperlink :: String -> HTMLProps
mkHyperlink s = mkAttrs [("href", s)]

instance Empty HTMLProps where
    empty = HTMLProps Nothing []

data HTML 
    = Root [HTML]
    | Div HTMLProps [HTML]
    | Heading Int HTMLProps String
    | Doctype String
    | Style CSS
    | Other String HTMLProps [HTML]
    | Html [HTML]
    | Head [HTML]
    | Body [HTML]
    | Text String
    | Image HTMLProps String String

instance Empty HTML where
  empty = Other "" empty []

instance Show HTML where
    show (Div props xs) = show $ Other "div" props xs
    show (Heading num props s) = show $ Other ("h" ++ show num) props [Text s]
    show (Html xs) = show $ Other "html" empty xs
    show (Head xs) = show $ Other "head" empty xs
    show (Body xs) = show $ Other "body" empty xs
    show (Other name props xs) = 
        let obj = showHTMLObject name props 
        in fst obj ++ concatMap show xs ++ snd obj

    show (Style css) = "<style>" ++ concatMap show css ++ "</style>"
    show (Doctype s) = "<!DOCTYPE " ++ s ++ ">"
    show (Root xs) = concatMap show xs
    show (Text s) = s
    show (Image props name alt) = show $ Other "img" (props <> mkAttrs [("src", name), ("alt", alt)]) []

showHTMLObject :: String -> HTMLProps -> (String, String)
showHTMLObject name (HTMLProps {cssClass, extraVars}) = 
  (
    concat ["<", name, " ", maybe "" (mkVar "class" . Just) cssClass, 
      concatMap (uncurry mkVar) extraVars, ">"],
    concat ["</", name, ">"]
  )

class ToHTML a where
  toHTML :: a -> HTML

instance ToHTML HTML where
  toHTML a = a

mkDiv :: (ToHTML a) => HTMLProps -> [a] -> HTML
mkDiv props xs = Div props $ map toHTML xs

instance ToHTML String where
  toHTML = Text

instance ToHTML Int where
  toHTML = toHTML . show

mkBulletList :: [HTML] -> HTML
mkBulletList = Other "ul" (mkAttrs [("style", "list-style-type:disc;")]) . map (Other "li" empty . (:[]))

-- width height (px)
mkSize :: Int -> Int -> HTMLProps
mkSize width height = mkAttrs [("width", px width), ("height", px height)]
  where
    px a = show a ++ "px"