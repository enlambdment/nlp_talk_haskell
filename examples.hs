module Examples where 

import L0

-- Examples phrases for each rule (p. 5)

-- NP -> Pronoun
i :: Pronoun 
i = mkPronoun "I"

-- NP -> ProperNoun
losAngeles :: ProperNoun 
losAngeles = mkProperNoun "Los Angeles"

a :: Det
a = mkDet "a"

flight :: Noun 
flight = mkNoun "flight"

-- Nominal -> Noun
flight' :: Nominal Noun 
flight' = MkNominal2 flight

-- NP -> Det Nominal
a_flight' :: NP (Det, Nominal Noun)
a_flight' = MkNP3 a flight'

-- Nominal -> Nominal Noun 
-- ('morning' is the nominal, 
--  'flight' is the noun.)
morning :: Noun 
morning = mkNoun "morning"

morning' :: Nominal Noun 
morning' = MkNominal2 morning 

--                 Nominal -> Nominal         Noun
-- So, one way to read this type signature is:
{-
        Nominal 
      /        \
    Nominal    Noun
    |          |
    Noun       'flight'
    |
    'morning'
-}
morning'_flight :: Nominal    (Nominal Noun,  Noun)
morning'_flight = MkNominal1 morning' flight

-- VP -> Verb 
do_v :: Verb 
do_v = mkVerb "do"

-- VP -> Verb NP 
want :: Verb 
want = mkVerb "want"

want_a_flight' :: VP (Verb, NP (Det, Nominal Noun))
want_a_flight' = MkVP2 want a_flight'

-- S -> NP VP 
i' :: NP Pronoun
i' = MkNP1 i

i'_want_a_flight' :: S (NP Pronoun,
                        VP (Verb, NP (Det, Nominal Noun)))
i'_want_a_flight' = MkS i' want_a_flight'

-- VP -> Verb NP PP
leave :: Verb 
leave = mkVerb "leave"

leave' :: VP Verb 
leave' = MkVP1 leave

boston :: ProperNoun
boston = mkProperNoun "Boston"

boston' :: NP ProperNoun 
boston' = MkNP2 boston 

the :: Det 
the = mkDet "the"

the_morning' :: NP (Det, Nominal Noun)
the_morning' = MkNP3 the morning'

in_p :: Prep 
in_p = mkPrep "in"

in_the_morning' :: PP (Prep, NP (Det, Nominal Noun))
in_the_morning' = MkPP1 in_p the_morning'

leave_boston'_in_the_morning' 
  :: VP (Verb,
         NP ProperNoun,
         PP ( Prep, 
              NP (Det, 
                  Nominal Noun)))
leave_boston'_in_the_morning' = 
  MkVP3 leave 
        boston'
        in_the_morning'   

-- VP -> Verb PP 
on :: Prep
on = mkPrep "on"

thursday :: Noun 
thursday = mkNoun "Thursday"

-- The grammar L0 given by Jurafsky is 
-- missing a rule! We cannot have the production
--  "NP -> Noun"
-- so instead we leverage MkNP3, which implements
-- the production
--  "NP -> Det Nominal"

thursday' :: Nominal Noun 
thursday' = MkNominal2 thursday

a_thursday' :: NP (Det, Nominal Noun)
a_thursday' = MkNP3 a thursday'

-- PP -> Prep NP
on_a_thursday' :: PP (Prep, NP (Det, Nominal Noun))
on_a_thursday' = MkPP1 on a_thursday'

leave_on_a_thursday' 
  :: VP (Verb,
         PP ( Prep, 
              NP (Det, 
                  Nominal Noun)))
leave_on_a_thursday' =
  MkVP4 leave on_a_thursday'         








