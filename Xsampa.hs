-- xsampa

module Xsampa 
      (xsampaToIpa) where 

import Data.Text (Text,pack,unpack,replace)
import Control.Lens ((&))

xsampaToIpa :: String -> String -- onyl works for vowels for now.
xsampaToIpa x = unpack $ map (uncurry replace) tables & foldl (&) (pack x)
    where tables = tableV

tableV :: [(Text,Text)]
tableV = map (\(x,y) -> (pack x,pack y))
         [ ("i","i")
         , ("y","y")
         , ("1","ɨ")
         , ("}","ʉ")
         , ("M","ɯ")
         , ("u","u")
         , ("I","ɪ")
         , ("Y","ʏ")
         , ("U","ʊ")
         , ("e","e")
         , ("2","ø")
         , ("@\\","ɘ")
         , ("8","ɵ")
         , ("7","ɤ")
         , ("o","o")
         , ("E","ɛ")
         , ("9","œ")
         , ("@","ə")
         , ("3\\","ɞ")
         , ("V","ʌ")
         , ("O","ɔ")
         , ("{","æ")
         , ("&","ɶ")
         , ("a","a")
         , ("A","ɑ")
         , ("Q","ɒ")
         ]

tableS :: [([Char], Char)]
tableS = [ ("m",'m')
         , ("n",'n')
         , ("n`",'ɳ')
         , ("J",'ɲ')
         , ("N",'ŋ')
         , ("l",'l')
         , ("l`",'ɭ')
         , ("L",'ʎ')
         , ("r",'r')
         , ("r\\",'ɹ')
         , ("4",'ɾ')
         ] 

tableO :: [([Char], Char)]
tableO = [ ("p",'p')
         , ("t",'t')
         , ("t`",'ʈ')
         , ("c",'c')
         , ("k",'k')
         , ("q",'q')
         , ("b",'b')
         , ("d",'d')
         , ("d`",'ɖ')
         , ("J\\",'ɟ')
         , ("g",'ɡ')
         , ("G\\",'ɢ')
         , ("f",'f')
         , ("T",'θ')
         , ("s",'s')
         , ("S",'ʃ')
         , ("C",'ç')
         , ("x",'x')
         , ("X",'χ')
         , ("X\\",'ħ')
         , ("v",'v')
         , ("D",'ð')
         , ("z",'z')
         , ("Z",'ʒ')
         , ("j\\",'ʝ')
         , ("G",'ɣ')
         , ("R",'ʁ')
         , ("?\\",'ʕ')
         ]