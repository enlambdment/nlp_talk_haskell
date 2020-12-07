{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}

-- All pages numbers are according to Ch. 12, "Context-Free Grammars"
--  of Speech and Language Processing (Jurafsky / Martin).

-- LEXICON AND GRAMMAR FOR L0, THE PHRASE-STRUCTURE GRAMMAR IN 
-- PGS. 1-5 

module L0 (
  Noun,       nounStrs,       mkNoun,
  Det,        detStrs,        mkDet,
  ProperNoun, properNounStrs, mkProperNoun,
  Pronoun,    pronounStrs,    mkPronoun,
  Verb,       verbStrs,       mkVerb,
  Prep,       prepStrs,       mkPrep,

  NP(..),
  Nominal(..),
  S(..),
  VP(..),
  PP(..)
  ) where 

-- Export smart constructors for Noun & other closed semantic classes.
-- In our toy implementation, we will only recognize a limited set of 
-- lexical items for each word class

------------------
-- NOUN WORD CLASS
------------------

data Noun = Noun {
  nounWord :: String
  } deriving Eq 

-- Custom show instances for all smart-constructed closed semantic classes,
-- to improve upon the record syntax.
-- We can even fake the underscore convention for labelling word with 
-- their class.
instance Show Noun where 
  show :: Noun -> String 
  show (Noun w) = w ++ "_N"

nounStrs :: [String]
nounStrs = [
  "flight", "breeze", "trip", "morning", "Thursday" ]

mkNoun :: String -> Noun
mkNoun str    | str `elem` nounStrs = Noun str 
              | otherwise           = error $ 
                      "Sorry, we couldn't recognize lexical item "
                      ++ str ++ " as a Noun."

{-
 detStrs, and the "smart constructor" mkDet below, together are saying that
  Det -> a
  Det -> the
  Det -> an 
    (...)
  expressing facts about the lexicon.
  The symbols corresponding to words in our language are *terminal symbols.*
  The symbols that express extractions over these terminals (i.e. the X's,
    X -> T
  in any rule) are called *non-terminals*.

  In any context-free rule, we must have:
    * the item to the *right* of the arrow (->) is an ordered list of one 
      or more terminals & non-terminals;
    * the item to the *left* of the arrow is a single non-terminal symbols
      expressing some cluster / generalization.
  e.g.
    N_j -> N_1 N_2 ... N_(j-1) T_1 T_2 ... T_k
  with the N's as non-terminals, the T's are terminals belonging to Σ
-}

-----------------
-- DET WORD CLASS
-----------------

data Det = Det { 
  detWord :: String 
  } deriving Eq 

instance Show Det where 
  show :: Det -> String 
  show (Det w) = w ++ "_DET"

detStrs :: [String]
detStrs = [
  "the", "a", "an", "this", "these", "that" ]

mkDet :: String -> Det 
mkDet str   | str `elem` detStrs = Det str 
            | otherwise          = error $ 
                "Sorry, we couldn't recognize lexical item "
                ++ str ++ " as a Det."

-------------------------
-- PROPER NOUN WORD CLASS
-------------------------

data ProperNoun = ProperNoun { 
  properNounWord :: String 
  } deriving Eq 

-- I don't know what the convention is for proper nouns.
instance Show ProperNoun where
  show :: ProperNoun -> String 
  show (ProperNoun w) = w ++ "_ProperN"

properNounStrs :: [String]
properNounStrs = [
  "Alaska", "Baltimore", "Los Angeles", 
  "Chicago", "United", "American", "Boston" ] 

mkProperNoun :: String -> ProperNoun 
mkProperNoun str    | str `elem` properNounStrs = ProperNoun str 
                    | otherwise                 = error $ 
                        "Sorry, we couldn't recognize lexical item "
                        ++ str ++ " as a ProperN."

---------------------
-- PRONOUN WORD CLASS
---------------------

data Pronoun = Pronoun {
  pronounWord :: String
  } deriving Eq 

instance Show Pronoun where 
  show :: Pronoun -> String
  show (Pronoun w) = w ++ "_PRON"

pronounStrs :: [String]
pronounStrs = [ 
  "me", "I", "you", "it" ]

mkPronoun :: String -> Pronoun 
mkPronoun str     | str `elem` pronounStrs = Pronoun str 
                  | otherwise                 = error $
                      "Sorry, we couldn't recognize lexical item " 
                      ++ str ++ " as a Pronoun."      

{-
 The most widely used formal system for modeling constituent structure
 in English and other natural languages is the Context-Free Grammar 
 (CFG) - a.k.a phrase-structure grammars.
 The formalism is equivalent to Backus-Naur Form (BNF).

 A context free grammar consists of 
  * a set of rules a.k.a. productions, each of which expresses 
    the ways that symbols of the language can be grouped and 
    ordered together, and 
  * a lexicon of words and symbols.

 e.g. the following productions expressing the different ways that an 
 NP (noun phrase) may be built:

  "An NP can be composed of either a determiner followed by a nominal"
 NP       -> Det Nominal
  "... or a ProperNoun"
 NP       -> ProperNoun 
  "In turn, a nominal can consist of one or more nouns."
 Nominal  -> Noun | Nominal Noun
-}

-- Data type for noun phrases. 
data NP a where 
  -- Data constructors for the NP data type.
  -- GADT's lang ext' lets us fully write out type sigs for our constructors.
  -- For each production (rule), exactly one data constructor.
  MkNP1 :: Pronoun -> NP Pronoun
  MkNP2 :: ProperNoun -> NP ProperNoun
  MkNP3 :: Det -> Nominal a -> NP (Det, Nominal a)
  

-- We need Show instances in order to print our phrase types.
-- We may define the Show instance for NP by pattern matching on all (both)
-- possible constructors for NP data.
instance Show (NP a) where 
  show :: NP a -> String 
  show (MkNP1 pronoun) = "(" ++ show pronoun ++ ")" ++ "_NP"
  show (MkNP2 propern) = "(" ++ show propern ++ ")" ++ "_NP"
  show (MkNP3 det nom) = "(" ++ show det ++ " " 
                             ++ show nom ++ ")" ++ "_NP"


-- Note the inductive nature of Nominal's structure.
data Nominal a where 
  MkNominal1 :: Nominal b -> Noun -> Nominal (Nominal b, Noun)
  MkNominal2 :: Noun -> Nominal Noun


instance Show (Nominal a) where 
  show :: Nominal a -> String 
  show (MkNominal1 nom noun) = "(" ++ show nom ++ " "
                                   ++ show noun ++ ")" ++ "_NOM"
  show (MkNominal2 noun) = "(" ++ show noun ++ ")" ++ "_NOM"


{-
  "Let’s add a few additional rules to our inventory. The following rule 
  expresses the fact that a sentence can consist of a noun phrase followed 
  by a verb phrase:"

    S -> NP VP          "I prefer a morning flight"
-}
data S t where 
  MkS :: NP a -> VP b -> S (NP a, VP b)

instance Show (S t) where
  show :: S t -> String 
  show (MkS np vp) = "(" ++ show np ++ " "
                         ++ show vp ++ ")" ++ "_S"

data VP t where 
  --               "do"
  MkVP1 :: Verb -> VP Verb

  -- "A verb phrase in English consists of a verb followed by assorted other 
  -- things; for example, one kind of verb phrase consists of a verb followed 
  -- by a noun phrase:"
  -- VP -> Verb    NP         "prefer a morning flight"
  MkVP2 :: Verb -> NP b    -> VP (Verb, NP b)

  -- "Or the verb may be followed by a noun phrase and a prepositional phrase:"
  -- VP -> Verb    NP       PP        "leave Boston in the morning"
  MkVP3 :: Verb -> NP b ->  PP pr  -> VP (Verb, NP b, PP pr)

  -- "Or the verb phrase may have a verb followed by a prepositional phrase alone:"
  -- VP -> Verb    PP                 "leave on Thursday"
  MkVP4 :: Verb -> PP pr          ->  VP (Verb, PP pr)

instance Show (VP t) where 
  show :: VP t -> String
  show (MkVP1 verb) = "(" ++ show verb ++ ")" ++ "_VP" 
  show (MkVP2 verb np) = "("  ++ show verb ++ " " 
                              ++ show np ++ ")" ++ "_VP"
  show (MkVP3 verb np pp) = "(" ++ show verb ++ " "
                                ++ show np ++ " "
                                ++ show pp ++ ")" ++ "_VP"
  show (MkVP4 verb pp) = "(" ++ show verb ++ " "
                             ++ show pp ++ ")" ++ "_VP"

-- We have to say what verbs are, for our example:
data Verb = Verb { 
  verbWord :: String 
  } deriving Eq 

instance Show Verb where 
  show :: Verb -> String 
  show (Verb w) = w ++ "_V"

verbStrs :: [String]
verbStrs = [
  "is", "prefer", "like", "need", "want", "fly", "do", "leave" ]

mkVerb :: String -> Verb
mkVerb str    | str `elem` verbStrs = Verb str 
              | otherwise           = error $ 
                      "Sorry, we couldn't recognize lexical item "
                      ++ str ++ " as a Verb."

-- A prepositional phrase generally has a preposition followed by a noun phrase.
-- For example, a common type of prepositional phrase in the ATIS corpus is used to
-- indicate location or direction:
data PP t where 
  -- PP -> Preposition     NP              "from Los Angeles"
  MkPP1 :: Prep         -> NP b         -> PP (Prep, NP b)

instance Show (PP t) where 
  show :: PP t -> String 
  show (MkPP1 prep np) = "(" ++ show prep ++ " "
                             ++ show np ++ ")" ++ "_PP"

data Prep = Prep {
  prepWord :: String
  } deriving Eq 

instance Show Prep where 
  show :: Prep -> String 
  show (Prep w) = w ++ "_P"

prepStrs :: [String]
prepStrs = [
  "from", "to", "on", "near", "in" ]

mkPrep :: String -> Prep
mkPrep str    | str `elem` prepStrs = Prep str 
              | otherwise           = error $ 
                      "Sorry, we couldn't recognize lexical item "
                      ++ str ++ " as a Prep."
