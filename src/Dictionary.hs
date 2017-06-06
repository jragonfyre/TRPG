--
-- Dictionary.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary where
--  (
--  ) where

import Dictionary.Types
import Dictionary.Instances
import Dictionary.Utils

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)


import Data.Char (isLetter)
import Control.Applicative ((<$>), (<*>), (<|>))
import Utils (allValues)
import Data.Maybe (mapMaybe, fromMaybe)


--import GHC.Generics

import Data.Monoid ((<>), Monoid (..))


-- note that this is all for English. Translating this would be a bitch.
-- We'll see if we can make this more generic later...


regularSimpleInflection :: SimpleInflection -> String -> String
regularSimpleInflection InflPlural = regularPlural
regularSimpleInflection InflThirdPersonPresent = addEss
regularSimpleInflection InflSimplePast = regularPast
regularSimpleInflection InflPastParticiple = regularPastParticiple
regularSimpleInflection InflPresentParticiple = regularPresentParticiple

simpleInflect :: String -> Maybe Irregularities -> SimpleInflection -> String
simpleInflect _ (Just IrregularCopula) InflPlural = "" -- just don't be stupid xD That's a bad note huh TODO fix lol
simpleInflect _ (Just IrregularCopula) InflThirdPersonPresent = "is"
simpleInflect _ (Just IrregularCopula) InflSimplePast = "was/were" -- technically this doesn't exist, which is why it
-- isn't a proper word xD lol fix TODO again I guess
-- this really shouldn't be called on the copula, but I'm defining it mostly for convenience in
-- 1) the type signature, and 
-- 2) getting the participles, which are the same across persons
simpleInflect _ (Just IrregularCopula) InflPastParticiple = "been"
simpleInflect _ (Just IrregularCopula) InflPresentParticiple = "being"
simpleInflect str (Just (IrregularSimple map)) infl =
  fromMaybe (regularSimpleInflection infl str) (M.lookup infl map)
simpleInflect str Nothing infl = regularSimpleInflection infl str

possibleInflectedPOSs :: POSType -> [POS]
possibleInflectedPOSs (NounT (SimpleNounBase Count numgen)) = [Adjective Possessive] <|> Noun . SimpleNoun
  <$> numgen
  -- <*> [Subjective, Objective]
possibleInflectedPOSs (NounT (SimpleNounBase Uncount _)) = return . Noun $ SimpleNoun NotCount
  -- <$> [Subjective, Objective]
possibleInflectedPOSs (PronounT (PronounBase _ _ numgen)) = Pronoun
  <$> numgen
  <*> allCases
possibleInflectedPOSs (AdjectiveT (DeterminerBase _ numgen)) = Adjective . Determiner <$> numgen
possibleInflectedPOSs (AdjectiveT (SimpleAdjectiveBase)) = return $ Adjective SimpleAdjective
possibleInflectedPOSs (VerbT _) = [Adjective PastParticiple, Adjective PresentParticiple]
  <|> (fmap Verb $ SimpleVerb
    <$> allMoods
    <*> allTenses
    <*> allVoices
    <*> allPersons
    <*> anyNumGen)
possibleInflectedPOSs AdverbT = return $ Adverb
possibleInflectedPOSs PrepositionT = return $ Preposition

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

addEss :: String -> String
addEss word = reverse . addEssHelper $ reverse word
  where
    addEssHelper wb@('x':_) = "se"++wb
    addEssHelper wb@('s':_) = "se"++wb
    addEssHelper wb@('h':'c':_) = "se"++wb
    addEssHelper wb@('h':'s':_) = "se"++wb
    addEssHelper wb1@('y':l:wb) 
      = if isConsonant l
        then "sei"++(l:wb)
        else 's':wb1
    -- default rule
    addEssHelper wb = 's':wb

regularPlural :: String -> String
regularPlural noun = reverse . regPluralHelper $ reverse noun
  where
    -- common irregulars, which we will incorporate into the rule
    -- because they are more common than not it seems
    regPluralHelper ('e':'f':nb) = "sev"++nb
    regPluralHelper ('f':nb) = "sev"++nb 
    regPluralHelper nb@('o':_) = "se"++nb
    regPluralHelper ('s':'u':nb) = 'i':nb
    regPluralHelper ('s':'i':nb) = "se" ++ nb
    regPluralHelper ('n':'o':nb) = 'a':nb
    -- regular rules
    regPluralHelper noun = addEss noun


regularPast :: String -> String
regularPast = regularPastParticiple 

regularPastParticiple :: String -> String
regularPastParticiple verb = reverse . regPPHelper $ reverse verb
  where
    regPPHelper vb@('e':_) = 'd':vb
    regPPHelper vb = "de"++vb

regularPresentParticiple :: String -> String
regularPresentParticiple verb = reverse . regPrPHelper $ reverse verb
  where
    regPrPHelper ('e':vb) = "gni" ++ vb
    regPrPHelper vb = "gni" ++ vb

-- validator
-- TODO implement
compatible :: POSType -> POS -> Bool
compatible _ _ = True

simpleInflectBE :: BaseEntry -> SimpleInflection -> String
simpleInflectBE BaseEntry{baseText = str, irregularities = irr} = simpleInflect str irr

-- takes a dictionary form word, its POS info, the desired inflection's form, and produces the inflected form.
-- returns Nothing if the POS and POSType are incompatible
-- doesn't check irregulars obvi
inflectString :: BaseEntry -> POS -> Maybe String
inflectString
  be@BaseEntry
    { partOfSpeechType = (NounT (SimpleNounBase Count _))
    }
  (Noun (SimpleNoun Plural)) = 
    Just $ simpleInflectBE be InflPlural
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Adjective PresentParticiple) =
    Just $ simpleInflectBE be InflPresentParticiple
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Adjective PastParticiple) =
    Just $ simpleInflectBE be InflPastParticiple
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Verb (SimpleVerb Indicative (Tense Present False False) ActiveVoice Third (Singular _))) =
    Just $ simpleInflectBE be InflThirdPersonPresent
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Verb (SimpleVerb Indicative (Tense Present False False) ActiveVoice Third NotCount)) =
    Just $ simpleInflectBE be InflThirdPersonPresent
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Verb (SimpleVerb Indicative (Tense Past False False) ActiveVoice _ _)) =
    Just $ simpleInflectBE be InflSimplePast
inflectString
  be@BaseEntry
    { baseText = verb
    , partOfSpeechType = (VerbT _)
    , irregularities = irr
    }
  (Verb (SimpleVerb Indicative t@Tense{isContinuous=True} ActiveVoice p ng)) = do
    helper <- inflectString copulaBase (Verb (SimpleVerb Indicative t{isContinuous=False} ActiveVoice p ng))
    let prespart = simpleInflect verb irr InflPresentParticiple
    return $ helper ++ " " ++ prespart
inflectString
  be@BaseEntry
    { baseText = verb
    , partOfSpeechType = (VerbT _)
    , irregularities = irr
    }
  (Verb (SimpleVerb Indicative t@Tense{isPerfect=True} ActiveVoice p ng)) = do
    helper <- inflectString helperHaveBase (Verb (SimpleVerb Indicative t{isPerfect=False} ActiveVoice p ng))
    let pastpart = simpleInflect verb irr InflPastParticiple
    return $ helper ++ " " ++ pastpart
-- most inflections in English are trivial,
-- so the default case will just check if they're compatible and return true if they are.
inflectString be pos = if compatible (partOfSpeechType be) pos then Just (baseText be) else Nothing


inflect :: BaseEntry -> POS -> Maybe InflectedEntry
inflect be pos = do
    text <- (inflectString be pos)
    return $ InflectedEntry text pos be

pastParticiplePOS :: POS
pastParticiplePOS = Adjective PastParticiple

simplePastTense :: Tense
simplePastTense = Tense Past False False

simplePresentTense :: Tense
simplePresentTense = Tense Present False False

simplePastPOS :: Person -> NumberGender -> POS
simplePastPOS p ng = Verb $ SimpleVerb Indicative simplePastTense ActiveVoice p ng

simplePresentPOS :: Person -> NumberGender -> POS
simplePresentPOS p ng = Verb $ SimpleVerb Indicative simplePresentTense ActiveVoice p ng

irregularThirdPersonPresent :: String -> Maybe Irregularities
irregularThirdPersonPresent = Just . IrregularSimple . M.singleton InflThirdPersonPresent
  
irregularSimplePast :: String -> Maybe Irregularities
irregularSimplePast = Just . IrregularSimple . M.singleton InflSimplePast

irregularPastParticiple :: String -> Maybe Irregularities
irregularPastParticiple = Just . IrregularSimple . M.singleton InflPastParticiple

copulaBase :: BaseEntry
copulaBase = BaseEntry
  { baseText = "be"
  , partOfSpeechType = (VerbT Copula)
  , irregularities = Just IrregularCopula
  }

helperHaveBase :: BaseEntry
helperHaveBase = BaseEntry
  { baseText = "have"
  , partOfSpeechType = (VerbT HelperVerb)
  , irregularities = 
      (irregularSimplePast "had") <> (irregularPastParticiple "had") <> (irregularThirdPersonPresent "has")
  }

generateEntries :: BaseEntry -> [InflectedEntry]
generateEntries be = mapMaybe (inflect be) . possibleInflectedPOSs $ partOfSpeechType be


getNominalNumberGender :: InflectedEntry -> Maybe NumberGender
getNominalNumberGender InflectedEntry{ partOfSpeech = Noun (SimpleNoun ng) } = Just ng
getNominalNumberGender InflectedEntry{ partOfSpeech = Pronoun ng _ } = Just ng
getNominalNumberGender _ = Nothing

--getVerbNumberGender :: InflectedEntry -> Maybe NumberGender
--getVerbNumberGender InflectedEntry{ partOfSpeech = Verb (

isInitialWordEntry :: WordEntry -> Bool
isInitialWordEntry we = case wordPosition we of
  Nothing ->
    True
  Just (1,_) ->
    True
  _ ->
    False

isFinalWordEntry :: WordEntry -> Bool
isFinalWordEntry we = case wordPosition we of
  Nothing ->
    True
  Just (x,y) ->
    x == y

isFollowingSameWordEntry :: WordEntry -> WordEntry -> Bool
isFollowingSameWordEntry first second = 
  (baseInflection first == baseInflection second)
    && (case wordPosition first of
      Nothing ->
        False
      Just (x1,m) ->
        case wordPosition second of
          Nothing ->
            False
          Just (x2, m) ->
            x2 == (x1 + 1)
       )

generateWords :: InflectedEntry -> [WordEntry]
generateWords inflEnt =
  let
    ws = words (entryText inflEnt)
    lw = length ws
  in case ws of 
    [oneWord] -> 
      [WordEntry oneWord inflEnt Nothing]
    _ ->
      map (\(singWord, pos) -> WordEntry singWord inflEnt (Just (pos,lw)))
        $ zip ws [1..]

generateDictionaryList :: [BaseEntry] -> [WordEntry]
generateDictionaryList bes = bes >>= generateEntries >>= generateWords

generateDictionary :: [BaseEntry] -> HashMap String [WordEntry]
generateDictionary bes = HM.fromListWith (++) (map (\we -> (wordText we, [we])) (generateDictionaryList bes))

getWordEntries :: (HashMap String [WordEntry]) -> String -> [WordEntry]
getWordEntries dict = fromMaybe [] . flip HM.lookup dict

toInflectionStreams :: [WordEntry] -> [[InflectedEntry]]
toInflectionStreams = toInflectionStreamsGen id (flip const)

toInflectionStreamsGen :: (a -> WordEntry) -> (a -> InflectedEntry -> b) -> [a] -> [[b]]
toInflectionStreamsGen unwr rewr xs = toIStreamHelper Nothing xs
  where
    toIStreamHelper Nothing [] = [[]] 
    toIStreamHelper (Just (f,x)) [] = 
      if isFinalWordEntry (unwr x)
      then
        [[]] 
      else
        []
    toIStreamHelper Nothing (x:xs) =
      let unx = unwr x
      in
        if isInitialWordEntry unx
        then
          if isFinalWordEntry unx
          then
            (rewr x (baseInflection unx):) <$> (toIStreamHelper Nothing xs)
          else
            toIStreamHelper (Just (x,x)) xs
        else
          []
    toIStreamHelper (Just (f,prev)) (x:xs) =
      let unx = unwr x
      in
        if isFollowingSameWordEntry (unwr prev) unx
        then
          if isFinalWordEntry unx
          then
            (rewr f (baseInflection unx):) <$> (toIStreamHelper Nothing xs)
          else
            toIStreamHelper (Just (f,x)) xs
        else
          []


