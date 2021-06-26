module SkriptParser.TriggerState where

import SkriptParser.Util
import Data.Set (Set, empty)
import Data.Typeable (TypeRep)
import Data.Maybe (listToMaybe)

data TriggerState = TriggerState {
  currentContexts :: Set TypeRep,
  currentSections :: [Todo],
  syntaxRestrictions :: [([TypeRep], Bool)]
}

emptyTriggerState :: TriggerState
emptyTriggerState = TriggerState empty [] [([],False)]

pushSection :: TriggerState -> Todo -> TriggerState
pushSection state section = state { currentSections = section : currentSections state }

popSection :: TriggerState -> TriggerState
popSection state = state { currentSections = tail (currentSections state) }

restricting :: TriggerState -> [TypeRep] -> Bool -> TriggerState
restricting state allowed expressionsRestricted = state {
  syntaxRestrictions = (allowed, expressionsRestricted) : syntaxRestrictions state
}

isRestricting :: TriggerState -> Bool
isRestricting state = snd `any` listToMaybe (syntaxRestrictions state)

clearRestrictions :: TriggerState -> TriggerState
clearRestrictions state = state { syntaxRestrictions = let (r:rs) = syntaxRestrictions state in if null rs then [r] else rs}

forbids :: TriggerState -> TypeRep -> Bool
forbids state syn = not ((elem syn . fst) `all` listToMaybe (syntaxRestrictions state))
