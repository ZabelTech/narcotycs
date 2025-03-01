module Naive.Constraints where

import Control.Monad (guard,unless)
import Control.Monad.Except (MonadError(..))
import Data.Either (partitionEithers)
import Data.List (intersect,nub)

data Fact = HasKey Constraint
          | HasNotKey Constraint
          | IsLiteral String
  deriving (Eq,Show)

data Constraint = By Fact
                | Union Constraint Constraint
                | Universe
  deriving (Eq,Show)

data ConstraintExpression = Expression {
  name       :: String,
  expression :: ([IsConstraint] -> Either ConstraintError IsConstraint) }

instance Show ConstraintExpression where
  show (Expression name _) = "Expression<" ++ name ++ ">"

instance Eq ConstraintExpression where
  (Expression lhs _) == (Expression rhs _) = lhs == rhs

data IsConstraint = IsIndex Constraint
                  | IsString Constraint
                  | IsFunction { args   :: [ConstraintExpression],
                                 result :: ConstraintExpression }
  deriving (Eq,Show)

data ConstraintType = Index | String | Function
  deriving (Eq,Show)

data ConstraintError = Conflicts    [(Fact,Fact)]
                     | NotSatisfied [Fact]
                     | UnexpectedType { expected :: ConstraintType,
                                        actual   :: ConstraintType }
  deriving (Eq,Show)

type ConstraintErrorM m = MonadError ConstraintError m

fromFacts :: [Fact] -> Constraint
fromFacts [] = Universe
fromFacts xs = foldl1 Union $ By <$> xs

toFacts :: Constraint -> [Fact]
toFacts Universe        = []
toFacts (By f)          = [f]
toFacts (Union lhs rhs) = nub $ toFacts lhs ++ toFacts rhs

getType :: IsConstraint -> ConstraintType
getType (IsIndex _)      = Index
getType (IsString _)     = String
getType (IsFunction _ _) = Function

isOpen :: Constraint -> Bool
isOpen Universe        = True
isOpen (Union lhs rhs) = isOpen lhs || isOpen rhs
isOpen _               = False

partitionFacts :: Constraint -> ([Constraint], [Constraint])
partitionFacts v = partitionFacts' $ toFacts v
  where partitionFacts' = partitionEithers . map (\case
          HasKey a    -> Right a
          HasNotKey a -> Left a)

getConflicts :: Constraint -> Constraint -> [(Fact,Fact)]
getConflicts lhs rhs =
  nub $ (rhsHasNot `conflicts` lhsHas) ++ (lhsHasNot `conflicts` rhsHas)
  where (lhsHasNot,lhsHas) = partitionFacts lhs
        (rhsHasNot,rhsHas) = partitionFacts rhs
        conflicts xs ys = do
          x <- xs `intersect` ys
          return (HasKey x, HasNotKey x)

getUnsatisfied :: Constraint -> Constraint -> [Fact]
getUnsatisfied lhs rhs = do
  x <- rhsHas
  guard $ x `notElem` lhsHas
  return $ HasKey x
  where (_,lhsHas) = partitionFacts lhs
        (_,rhsHas) = partitionFacts rhs

unifyFacts :: ConstraintErrorM m => Constraint -> Constraint -> m Constraint
unifyFacts lhs rhs
  | not $ null conflicts = throwError $ Conflicts conflicts
  | isOpen lhs           = return $ Union lhs rhs
  | null unsatisfied     = return lhs
  | otherwise            = throwError $ NotSatisfied unsatisfied
  where unsatisfied = getUnsatisfied lhs rhs
        conflicts   = getConflicts lhs rhs

sumFacts :: ConstraintErrorM m => Constraint -> Constraint -> m Constraint
sumFacts lhs rhs = do
  unless (null conflicts) $ throwError $ Conflicts conflicts
  return $ Union lhs rhs
  where conflicts = getConflicts lhs rhs

combineConstraints :: ConstraintErrorM m => IsConstraint -> IsConstraint -> m IsConstraint
combineConstraints (IsIndex lhs) (IsIndex rhs) = IsIndex <$> unifyFacts lhs rhs
combineConstraints (IsString Universe) rhs@(IsString _) = return rhs
combineConstraints lhs@(IsString _) (IsString Universe) = return lhs
combineConstraints lhs rhs = throwError $ UnexpectedType (getType lhs) (getType rhs)
