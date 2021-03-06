module House (rhyme) where
import Data.List

rhyme :: String
rhyme = unlines . reverse $ verses
  where 
    verses = combine <$> slices songLines
    combine = ("This is " ++) . concat
slices = init . tails
songLines = [
        "the horse and the hound and the horn\nthat belonged to ",
        "the farmer sowing his corn\nthat kept ",
        "the rooster that crowed in the morn\nthat woke ",
        "the priest all shaven and shorn\nthat married ",
        "the man all tattered and torn\nthat kissed ",
        "the maiden all forlorn\nthat milked ",
        "the cow with the crumpled horn\nthat tossed ",
        "the dog\nthat worried ",
        "the cat\nthat killed " ,
        "the rat\nthat ate ",
        "the malt\nthat lay in " ,
        "the house that Jack built.\n"
      ]

