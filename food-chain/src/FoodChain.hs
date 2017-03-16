module FoodChain (song) where
import Text.Printf
import Data.List

song :: String
song = beginning ++ middle ++ end
  where
    beginning = "I know an old lady who swallowed a fly.\n" ++
                "I don't know why she swallowed the fly. Perhaps she'll die.\n\n" ++

                "I know an old lady who swallowed a spider.\n" ++
                "It wriggled and jiggled and tickled inside her.\n" ++
                "She swallowed the spider to catch the fly.\n" ++
                "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"

    middle = unlines . tail . fmap block $ slices
      where 
         slices  = reverse . init . tails $ [("cow",  "I don't know how she swallowed a cow!\n"),
                                              ("goat", "Just opened her throat and swallowed a goat!\n"),
                                              ("dog",  "What a hog, to swallow a dog!\n"),
                                              ("cat",  "Imagine that, to swallow a cat!\n"),
                                              ("bird", "How absurd to swallow a bird!\n"),
                                              ("spider that wriggled and jiggled and tickled inside her", "")
                                              ]

    end       = "I know an old lady who swallowed a horse.\n" ++
                "She's dead, of course!\n\n"


block w = start ++ tagline ++ rest ++ end
  where
    tagline = snd . head $ w
    start = printf "I know an old lady who swallowed a %s.\n" $ fst . head $ w
    rest    = unlines $ swallowed w
    end = "She swallowed the spider to catch the fly.\n"++ 
          "I don't know why she swallowed the fly. Perhaps she'll die.\n"

swallowed = (zipWith format =<< tail) . map fst
 where format = (flip $ printf "She swallowed the %s to catch the %s.")

