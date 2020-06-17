-- IPA
-- If you are on Windows, run this before starting the program: 
-- chcp 65001

{-# LANGUAGE TemplateHaskell #-} --
{- {-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE InstanceSigs #-} -}

import Prelude hiding (round)
import Control.Lens
import Data.Maybe (isJust,fromJust,maybeToList)
import Data.Tuple (swap)
import Xsampa 

data Manner = Nasal 
            | Plosive 
            | Fricative
            | Affricate-- experimental
            | Approximant 
            -- glides?
            | Trill 
            | Flap 
            | LateralFricative 
            | LateralAproximant 
            | LateralFlap deriving (Eq,Show)

data Place = Bilabial
           | LabioDental
           | Dental
           | Alveolar 
           | PostAlveolar
           | Retrofelx
           | Palatal
           | Velar
           | Uvular
           | Pharyngal
           | EpiGlottal
           | Glottal deriving (Eq,Show)

data Voice = Voiced
           | Voiceless deriving (Eq,Show)

data Height = HighTense
            | HighLax
            | MidTense
            | MidLax
            | Low deriving (Eq,Show)      
           
data Frontness = Front
               | Central 
               | Back deriving (Eq,Show)

data Roundness = Round
               | Unround deriving (Eq,Show)

data Description = Consonant Voice Place Manner -- implement Gemminates!
                 | Vowel Roundness Frontness Height -- implement Length!
--                 | DoublePlosive Voice Place Place -- experimental
--                 | DoubleStop Voice Place Place    -- experimental -- difference Stop/Plosive?
--                 | Click Place                     -- experimental
--                 | Implosive Place                 -- experimental
--                 | Ejective Place                  -- experimental 
                 deriving (Eq,Show)

data Specification = S { 
{- manner -}             _syllabic :: Maybe Bool
                       , _consonantal :: Maybe Bool
                       , _appr :: Maybe Bool
                       , _son :: Maybe Bool
                       , _cont :: Maybe Bool
                       , _delayed :: Maybe Bool
                       , _tap :: Maybe Bool
                       , _trill :: Maybe Bool
                       , _nasal :: Maybe Bool
{- laryngal -}         , _voice :: Maybe Bool
                       , _spread :: Maybe Bool
                       , _constricted :: Maybe Bool
                    -- , _implosive :: Maybe Bool
{- place -}            , _labial :: Maybe Bool
                       , _round :: Maybe Bool
                       , _labioDental :: Maybe Bool
                       , _coronal :: Maybe Coronal
                       , _lateral :: Maybe Bool
                       , _dorsal :: Maybe Dorsal
                       } deriving (Eq, Show)

data Coronal = Coronal { _ant :: Maybe Bool
                       , _dist :: Maybe Bool
                       , _strident :: Maybe Bool
                       } deriving (Eq, Show) 

data Dorsal = Dorsal { _high :: Maybe Bool
                     , _low :: Maybe Bool
                     , _front :: Maybe Bool
                     , _back :: Maybe Bool
                     , _tense :: Maybe Bool
                     } deriving (Eq, Show)

makeLenses ''Coronal 
makeLenses ''Dorsal
makeLenses ''Specification

(§) :: (a1 -> b -> a2 -> a2) -> [(a1, b)] -> a2 -> a2
(§) func vals spec = foldl (&) spec $ map (uncurry func) vals

setDor :: ((a -> Identity b) -> Dorsal -> Identity Dorsal) -> b -> Specification -> Specification
setDor feature value = dorsal . _Just . feature .~ value

overDor :: ((a -> Identity b) -> Dorsal -> Identity Dorsal) -> (a -> b) -> Specification -> Specification
overDor feature value = dorsal . _Just . feature %~ value

setCor :: ((a -> Identity b) -> Coronal -> Identity Coronal) -> b -> Specification -> Specification
setCor feature value = coronal . _Just . feature .~ value

blank :: Specification
blank = S Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
blankVowel :: Specification
blankVowel = blank & set § values & dorsal .~ Just (Dorsal Nothing Nothing Nothing Nothing Nothing)
    where values = map (fmap pure) [ (syllabic,True),(consonantal,False),(son,True),(cont,True),(appr,True)
                                   , (tap,False),(trill,False),(nasal,False),(voice,True),(spread,False)
                                   , (constricted,False),(labioDental,False)] 


conJust :: a -> b -> Maybe a
conJust = const . Just 

desToSpec :: Description -> Specification
desToSpec (Vowel roundness front' height) = blankVowel & overDor § dorsals & set § labials 
    where dorsals = 
                    [ (high,conJust $ height `elem` [HighTense,HighLax])
                    , (low,conJust $ height == Low)
                    , (tense, case height of Low -> const Nothing;
                                               _ -> conJust $ height `elem` [HighTense,MidTense])
                    , (front,conJust $ front' == Front)
                    , (back,conJust $ front' == Back)]
          labials = map (fmap pure) [(labial,roundness == Round),(round,roundness == Round)]
desToSpec (Consonant voiced place manner) = blank & set § general
    where general = [ (syllabic,Just False), (voice,Just $ voiced == Voiced)
                    ]

specToDes :: Specification -> Description
specToDes = undefined

charToDes :: Char -> Maybe Description
charToDes = flip lookup vowels

desToChar :: Description -> Maybe Char
desToChar = flip lookup (map swap vowels)

stringToDes :: [Char] -> [Description]
stringToDes dess = dess >>= maybeToList . charToDes 

desToString :: [Description] -> [Char]
desToString dess = dess >>= maybeToList . desToChar 

vowels :: [(Char, Description)]
vowels = [('i',Vowel Unround Front   HighTense)
         ,('y',Vowel Round   Front   HighTense)
         ,('ɨ',Vowel Unround Central HighTense)
         ,('ʉ',Vowel Round   Central HighTense)
         ,('ɯ',Vowel Unround Back    HighTense)
         ,('u',Vowel Round   Back    HighTense)
         ,('ɪ',Vowel Unround Front   HighLax)
         ,('ʏ',Vowel Round   Front   HighLax)
         ,('ʊ',Vowel Round   Back    HighLax)
         ,('e',Vowel Unround Front   MidTense)
         ,('ø',Vowel Round   Front   MidTense)
         ,('ɘ',Vowel Unround Central MidTense)
         ,('ɵ',Vowel Round   Central MidTense)
         ,('ɤ',Vowel Unround Back    MidTense)
         ,('o',Vowel Round   Back    MidTense)
         ,('ɛ',Vowel Unround Front   MidLax)
         ,('œ',Vowel Round   Front   MidLax)
         ,('ə',Vowel Unround Central MidLax)
         ,('ɞ',Vowel Round   Central MidLax)
         ,('ʌ',Vowel Unround Back    MidLax)
         ,('ɔ',Vowel Round   Back    MidLax)
         ,('æ',Vowel Unround Front   Low)
         ,('ɶ',Vowel Round   Front   Low)
         ,('a',Vowel Unround Central Low)
         ,('ɑ',Vowel Unround Back    Low)
         ,('ɒ',Vowel Round   Back    Low)
         ]
