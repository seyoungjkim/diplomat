{-# OPTIONS -Wincomplete-patterns #-}
module BuildQuestion where
import Question

import qualified Data.Maybe as Maybe

-------------------------------------------------------------------------
buildQuestion :: Question -> Question -> Maybe Question
buildQuestion Blank q = Just q
buildQuestion (Union q1 q2) q = case buildQuestion q1 q of
  Just q1' -> Just $ Union q1' q2
  Nothing -> case buildQuestion q2 q of
    Just q2' -> Just $ Union q1 q2'
    Nothing -> Nothing
buildQuestion (Intersection q1 q2) q = case buildQuestion q1 q of
  Just q1' -> Just $ Intersection q1' q2
  Nothing -> case buildQuestion q2 q of
    Just q2' -> Just $ Intersection q1 q2'
    Nothing -> Nothing
buildQuestion (Not q1) q2 = case buildQuestion q1 q2 of
  Just q1' -> Just $ Not q1'
  Nothing -> Nothing
buildQuestion q _ = Nothing

buildQuestionWithQInt :: Question -> QInt -> Maybe Question
buildQuestionWithQInt (Union q1 q2) qi = 
  case buildQuestionWithQInt q1 qi of
    Just q1' -> Just $ Union q1' q2
    Nothing -> case buildQuestionWithQInt q2 qi of
                    Just q2' -> Just $ Union q1 q2'
                    Nothing -> Nothing
buildQuestionWithQInt (Intersection q1 q2) qi = 
  case buildQuestionWithQInt q1 qi of
    Just q1' -> Just $ Intersection q1' q2
    Nothing -> case buildQuestionWithQInt q2 qi of
                    Just q2' -> Just $ Intersection q1 q2'
                    Nothing -> Nothing                    
buildQuestionWithQInt (Not q) qi = 
  case buildQuestionWithQInt q qi of
    Just q' -> Just $ Not q'
    Nothing -> Nothing                                                              
buildQuestionWithQInt (Equals qi1 qi2) qi =
  case buildQuestionWithQInt' qi1 qi of
    Just qi1' -> Just $ Equals qi1' qi2
    Nothing -> case buildQuestionWithQInt' qi2 qi of
      Just qi2' -> Just $ Equals qi1 qi2'
      Nothing -> Nothing
buildQuestionWithQInt (Gt qi1 qi2) qi =
  case buildQuestionWithQInt' qi1 qi of
    Just qi1' -> Just $ Gt qi1' qi2
    Nothing -> case buildQuestionWithQInt' qi2 qi of
      Just qi2' -> Just $ Gt qi1 qi2'
      Nothing -> Nothing
buildQuestionWithQInt (Lt qi1 qi2) qi =
  case buildQuestionWithQInt' qi1 qi of
    Just qi1' -> Just $ Lt qi1' qi2
    Nothing -> case buildQuestionWithQInt' qi2 qi of
      Just qi2' -> Just $ Lt qi1 qi2'
      Nothing -> Nothing
buildQuestionWithQInt (Ge qi1 qi2) qi =
  case buildQuestionWithQInt' qi1 qi of
    Just qi1' -> Just $ Ge qi1' qi2
    Nothing -> case buildQuestionWithQInt' qi2 qi of
      Just qi2' -> Just $ Ge qi1 qi2'
      Nothing -> Nothing
buildQuestionWithQInt (Le qi1 qi2) qi =
  case buildQuestionWithQInt' qi1 qi of
    Just qi1' -> Just $ Le qi1' qi2
    Nothing -> case buildQuestionWithQInt' qi2 qi of
      Just qi2' -> Just $ Le qi1 qi2'
      Nothing -> Nothing
buildQuestionWithQInt _ _ = Nothing

buildQuestionWithQInt' :: QInt -> QInt -> Maybe QInt
buildQuestionWithQInt' BlankQInt qi = Just qi
buildQuestionWithQInt' (Sum qi1 qi2) qi = case buildQuestionWithQInt' qi1 qi of
  Just qi1' -> Just $ Sum qi1' qi2
  Nothing -> case buildQuestionWithQInt' qi2 qi of
    Just qi2' -> Just $ Sum qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQInt' (Diff qi1 qi2) qi = case buildQuestionWithQInt' qi1 qi of
  Just qi1' -> Just $ Diff qi1' qi2
  Nothing -> case buildQuestionWithQInt' qi2 qi of
    Just qi2' -> Just $ Diff qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQInt' (Mod qi1 qi2) qi = case buildQuestionWithQInt' qi1 qi of
  Just qi1' -> Just $ Mod qi1' qi2
  Nothing -> case buildQuestionWithQInt' qi2 qi of
    Just qi2' -> Just $ Mod qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQInt' (Product qi1 qi2) qi = case buildQuestionWithQInt' qi1 qi of
  Just qi1' -> Just $ Product qi1' qi2
  Nothing -> case buildQuestionWithQInt' qi2 qi of
    Just qi2' -> Just $ Product qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQInt' (Quotient qi1 qi2) qi = case buildQuestionWithQInt' qi1 qi of
  Just qi1' -> Just $ Quotient qi1' qi2
  Nothing -> case buildQuestionWithQInt' qi2 qi of
    Just qi2' -> Just $ Quotient qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQInt' _ _ = Nothing

buildQuestionWithQHand :: Question -> QHand -> Maybe Question
buildQuestionWithQHand (NonEmpty qh1) qh = case buildQuestionWithQHand'' qh1 qh of
  Just qh2 -> Just $ NonEmpty qh2
  Nothing -> Nothing
buildQuestionWithQHand (Union q1 q2) qh = case buildQuestionWithQHand q1 qh of
  Just q1' -> Just $ Union q1' q2
  Nothing -> case buildQuestionWithQHand q2 qh of
    Just q2' -> Just $ Union q1 q2'
    Nothing -> Nothing
buildQuestionWithQHand (Intersection q1 q2) qh = case buildQuestionWithQHand q1 qh of
  Just q1' -> Just $ Intersection q1' q2
  Nothing -> case buildQuestionWithQHand q2 qh of
    Just q2' -> Just $ Intersection q1 q2'
    Nothing -> Nothing
buildQuestionWithQHand (Not q1) qh = case buildQuestionWithQHand q1 qh of
  Just q2 -> Just $ Not q2
  Nothing -> Nothing
buildQuestionWithQHand (Equals qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
  Just qi1' -> Just $ Equals qi1' qi2
  Nothing -> case buildQuestionWithQHand' qi2 qh of
    Just qi2' -> Just $ Equals qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQHand (Gt qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
  Just qi1' -> Just $ Gt qi1' qi2
  Nothing -> case buildQuestionWithQHand' qi2 qh of
    Just qi2' -> Just $ Gt qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQHand (Lt qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
  Just qi1' -> Just $ Lt qi1' qi2
  Nothing -> case buildQuestionWithQHand' qi2 qh of
    Just qi2' -> Just $ Lt qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQHand (Ge qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
  Just qi1' -> Just $ Ge qi1' qi2
  Nothing -> case buildQuestionWithQHand' qi2 qh of
    Just qi2' -> Just $ Ge qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQHand (Le qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
  Just qi1' -> Just $ Le qi1' qi2
  Nothing -> case buildQuestionWithQHand' qi2 qh of
    Just qi2' -> Just $ Le qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQHand _ _ = Nothing

buildQuestionWithQHand' :: QInt -> QHand -> Maybe QInt
buildQuestionWithQHand' (Cardinality qh1) qh = case buildQuestionWithQHand'' qh1 qh of
  Just qh2 -> Just $ Cardinality qh2
  Nothing -> Nothing
buildQuestionWithQHand' (SumHand qh1) qh = case buildQuestionWithQHand'' qh1 qh of
  Just qh2 -> Just $ SumHand qh2
  Nothing -> Nothing
buildQuestionWithQHand' (ProductHand qh1) qh = case buildQuestionWithQHand'' qh1 qh of
  Just qh2 -> Just $ ProductHand qh2
  Nothing -> Nothing
buildQuestionWithQHand' (Sum qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
  Just qi1' -> Just $ Sum qi1' qi2
  Nothing -> case buildQuestionWithQHand' qi2 qh of
    Just qi2' -> Just $ Sum qi1 qi2'
    Nothing -> Nothing
buildQuestionWithQHand' (Diff qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
    Just qi1' -> Just $ Diff qi1' qi2
    Nothing -> case buildQuestionWithQHand' qi2 qh of
        Just qi2' -> Just $ Diff qi1 qi2'
        Nothing -> Nothing
buildQuestionWithQHand' (Mod qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
    Just qi1' -> Just $ Mod qi1' qi2
    Nothing -> case buildQuestionWithQHand' qi2 qh of
        Just qi2' -> Just $ Mod qi1 qi2'
        Nothing -> Nothing
buildQuestionWithQHand' (Product qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
    Just qi1' -> Just $ Product qi1' qi2
    Nothing -> case buildQuestionWithQHand' qi2 qh of
        Just qi2' -> Just $ Product qi1 qi2'
        Nothing -> Nothing
buildQuestionWithQHand' (Quotient qi1 qi2) qh = case buildQuestionWithQHand' qi1 qh of
    Just qi1' -> Just $ Quotient qi1' qi2
    Nothing -> case buildQuestionWithQHand' qi2 qh of
        Just qi2' -> Just $ Quotient qi1 qi2'
        Nothing -> Nothing
buildQuestionWithQHand' _ _ = Nothing

buildQuestionWithQHand'' :: QHand -> QHand -> Maybe QHand
buildQuestionWithQHand'' BlankQHand qh = Just qh
buildQuestionWithQHand'' (Filter f qh1) qh = case buildQuestionWithQHand'' qh1 qh of
  Just qh1' -> Just $ Filter f qh1'
  Nothing -> Nothing
buildQuestionWithQHand'' (UnionHand qh1 qh2) qh = case buildQuestionWithQHand'' qh1 qh of
    Just qh1' -> Just $ UnionHand qh1' qh2
    Nothing -> case buildQuestionWithQHand'' qh2 qh of
        Just qh2' -> Just $ UnionHand qh1 qh2'
        Nothing -> Nothing
buildQuestionWithQHand'' (IntersectionHand qh1 qh2) qh = case buildQuestionWithQHand'' qh1 qh of
    Just qh1' -> Just $ IntersectionHand qh1' qh2
    Nothing -> case buildQuestionWithQHand'' qh2 qh of
        Just qh2' -> Just $ IntersectionHand qh1 qh2'
        Nothing -> Nothing
buildQuestionWithQHand'' _ _ = Nothing


-- returning 0 for no blank, 1 for Blank, 2 for BlankQInt, 3 for BlankQHand
-- should think about changing this
findBlank :: Question -> Int
findBlank Blank = 1
findBlank (NonEmpty qh) = findBlankHand qh
findBlank (Union q1 q2) = 
  case findBlank q1 of
    0 -> findBlank q2
    x -> x
findBlank (Intersection q1 q2) =
  case findBlank q1 of
    0 -> findBlank q2
    x -> x
findBlank (Not q) = findBlank q
findBlank (Equals q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlank (Gt q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlank (Ge q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlank (Lt q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlank (Le q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlank (SpecificCard _) = 0

findBlankInt :: QInt -> Int
findBlankInt BlankQInt = 2
findBlankInt (Cardinality qh) = findBlankHand qh
findBlankInt (SumHand qh) = findBlankHand qh
findBlankInt (ProductHand qh) = findBlankHand qh
findBlankInt (Sum q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlankInt (Diff q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlankInt (Mod q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlankInt (Product q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlankInt (Quotient q1 q2) = 
  case findBlankInt q1 of
    0 -> findBlankInt q2
    x -> x
findBlankInt (IntVal _) = 0

findBlankHand :: QHand -> Int
findBlankHand BlankQHand = 3
findBlankHand (Filter _ qh) = findBlankHand qh
findBlankHand (UnionHand q1 q2) = 
  case findBlankHand q1 of
    0 -> findBlankHand q2
    x -> x
findBlankHand (IntersectionHand q1 q2) = 
  case findBlankHand q1 of
    0 -> findBlankHand q2
    x -> x
findBlankHand Hand = 0