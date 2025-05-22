module Main where

import CSS
import HTML
import Lib

import System.Directory
import System.FilePath

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let cacheDir = homeDir </> ".cache" </> "portfolio-webserver"
  let filePaths = map (cacheDir </>) ["Main.hs", "Lib.hs", "HTML.hs", "CSS.hs"]
  createDirectoryIfMissing True cacheDir
  result <- traverse doesFileExist filePaths
  if and result then do
    files <- traverse readFile filePaths
    case files of
      [mainStr, libStr, htmlStr, cssStr] -> do
        let fileStruct = Files mainStr libStr htmlStr cssStr
        print $
          Root [
            Doctype "html",
            Html [
              Head [
                Style cssBody
              ],
              Body [
                htmlBody fileStruct
              ]
            ]
          ]
      _ -> print "Unexpected file contents!"
    else print "Files don't exist!"

data Files = Files
  { mainFile :: String
  , libFile :: String
  , htmlFile :: String
  , cssFile :: String
  }

githubURL :: String
githubURL = "https://github.com/Pickles888"

data Section = Section
  {
    title :: String,
    content :: [HTML],
    extraProps :: HTMLProps
  }

mkSection :: (ToHTML a) => HTMLProps -> String -> [a] -> HTML
mkSection props s xs = toHTML $ Section {title = s, content = map toHTML xs, extraProps = props}

instance ToHTML Section where
  toHTML :: Section -> HTML
  toHTML (Section {title, content, extraProps}) =
    Div (mkClass "section" <> extraProps) $ Heading 1 empty title : content

htmlBody :: Files -> HTML
htmlBody (Files {mainFile, libFile, htmlFile, cssFile}) =
  mkDiv (mkClass "root") [
    Heading 1 empty "Holden Buzick's Portfolio",
    mkSection empty "Skills" [
      mkBulletList $ map Text [
        "Programming",
        "Robotics",
        "Middle School Diploma lmao",
        "Making coffee ig (great unpaid intern)"
      ]
    ],

    mkSection empty "Headshot" [
      Div (mkClass "centered") [
        Image empty "assets/holden.jpg" "Holden Headshot"
      ]
    ],

    mkSection empty "Contact" $ (:[]) $
      Div (mkClass "centered") $
        map (\(url, img, alt) ->
            Other "a" (mkHyperlink url <> mkAttr ("style", "margin:10px;flex-direction:column;")) [
              Image ((\a -> mkSize a a) 200) img alt
            ]
        ) [
          (githubURL, "assets/github.png", "Github"),
          ("mailto:holdenbuzick.com", "assets/thunderbird.png", "Email"),
          ("tel:+14154400390", "assets/phone.png", "Phone")
        ],

    mkSection empty "About Me" $ map (\a -> Other "p" empty [a]) [
      Text $ unwords [
        "I am an annoying programmer who wont stop yapping about random stuff.",
        "I am a functional programming supremacist and am interested",
        "in learning type theory and working with programming languages and PLT.",
        "I also want to learn things like proof helpers such as LEAN and working with proofs of programs."
      ],

      Text $ unwords [
        "In my freetime I play video games, browse wikipedia (genuinely an addiction), watch youtube, learn langauges, and sleep mostly.",
        "I also occasionally hang out with friends.",
        "I also work on robotics in my freetime."
      ]
    ],

    mkSection empty "Projects" [
      mkBulletList $
        map (\(text, link) -> Other "a" (maybe empty mkHyperlink link) [Text text]) [
          ("Haskell LEDS Simulator", Just "https://github.com/Pickles888/haskell-leds"),
          ("Todo List Written in Iced", Just "https://github.com/Pickles888/iced-todo"),
          ("Interpretted Lisp & Haskell Inspired programming language (WIP)", Nothing)
        ]
    ],

    mkSection empty "This Project!" [
      Other "details" empty (Other "summer" empty [] : map (\(title, code) ->
        Div empty [
          Heading 3 empty title,
          Div (mkClass "code-block") $ (:[]) $ Other "code" (mkClass "multiline") [Text ("\n" ++ code)]
        ]) [
          ("Main.hs", mainFile),
          ("Lib.hs", libFile),
          ("HTML.hs", htmlFile),
          ("CSS.hs", cssFile)
        ])
    ],

    mkSection empty "Experience" [
      mkBulletList [
        Text "First Robotics Competition (8852!!!!): Java, working in a large project with others.",
        Text "Experience working on coding projects in Java, Rust, Python, and Haskell",
        Text "AP CSA (if that counts as anything)"
      ]
    ],

    mkSection empty "Education" $ map (\a -> Other "p" empty [a]) [
      Text "I'm a softmore in highschool, going into junior year.",
      Text "I also (very crazy) have a middle school diploma!"
    ]
  ]

cssBody :: CSS
cssBody =
  [
    CSSObject "body" [
      mkDef "background-image" "url(\"assets/holden.jpg\")"
    ],

    CSSObject ".root" [
      mkDef "margin" "50px",
      mkDef "opacity" "0.8",
      mkDef "margin-left" "200px",
      mkDef "margin-right" "200px"
    ],

    CSSObject "h1" [
      mkDef "color" "blue",
      mkDef "background-color" "white",
      mkDef "padding" "5px"
    ],

    CSSObject "h3" [
      mkDef "color" "purple",
      mkDef "padding" "5px"
    ],

    CSSObject ".section" [
      mkDef "color" "black",
      mkDef "padding" "10px",
      mkDef "background-color" "white",
      mkDef "margin-top" "10px"
    ],

    CSSObject ".centered" [
      mkDef "display" "flex",
      mkDef "align-items" "center",
      mkDef "justify-content" "center"
    ],

    CSSObject ".multiline" [
      mkDef "white-space" "pre-wrap"
    ],

    CSSObject ".code-block" [
      mkDef "background-color" "#282828",
      mkDef "margin" "10px",
      mkDef "padding" "10px"
    ],

    CSSObject "code" [
      mkDef "color" "#fbf1c7"
    ]
  ]

expandableCSS :: CSS
expandableCSS =
  [
    CSSObject "details" [
      mkDef "user-select" "none"
    ],

    CSSObject "details>summary span.icon" [
      mkDef "width" "24px",
      mkDef "height" "24px",
      mkDef "transition" "all 0.3s",
      mkDef "margin-left" "auto"
    ],

    CSSObject "details[open] summary span.icon" [
      mkDef "transform" "rotate(180deg)"
    ],

    CSSObject "summary" [
      mkDef "display" "flex",
      mkDef "cursor" "pointer"
    ],

    CSSObject "summary::-webkit-details-marker" [
      mkDef "display" "none"
    ]
  ]