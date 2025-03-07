module Naive.Constraints where

import Control.Monad (guard)
import Control.Monad.Except (MonadError(..))
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List (intersect,nub)

data NaiveType = Index | String | Function
  deriving (Eq,Show)

data Fact (a :: NaiveType) where
  HasKey     :: Constraint 'String -> Fact 'Index
  HasNotKey  :: Constraint 'String -> Fact 'Index
  IsLiteral  :: String -> Fact 'String
  Expression :: { name       :: String,
                  expression :: [IsConstraint] -> Either ConstraintError IsConstraint
                } -> Fact 'Function

deriving instance Eq (Fact 'String)
deriving instance Eq (Fact 'Index)
deriving instance Show (Fact 'String)
deriving instance Show (Fact 'Index)

instance Show (Fact 'Function) where
  show (Expression name _) = "Expression<" ++ name ++ ">"

instance Eq (Fact 'Function) where
  (Expression lhs _) == (Expression rhs _) = lhs == rhs

data Constraint a = By (Fact a)
                  | Union (Constraint a) (Constraint a)
                  | Universe

deriving instance Eq (Constraint 'String)
deriving instance Eq (Constraint 'Index)
deriving instance Eq (Constraint 'Function)
deriving instance Show (Constraint 'String)
deriving instance Show (Constraint 'Index)
deriving instance Show (Constraint 'Function)

data IsConstraint = IsIndex (Constraint 'Index)
                  | IsString (Constraint 'String)
                  | IsFunction [Constraint 'Function] (Constraint 'Function)
  deriving (Eq,Show)

data ConstraintError = forall (a :: NaiveType). Show (Fact a) => Conflicts    [(Fact a,Fact a)]
                     | forall (a :: NaiveType). Show (Fact a) => NotSatisfied [Fact a]
                     | UnexpectedType { expected :: NaiveType,
                                        actual   :: NaiveType }

deriving instance Show ConstraintError

type ConstraintErrorM m = MonadError ConstraintError m

fromFacts :: [Fact a] -> Constraint a
fromFacts [] = Universe
fromFacts xs = foldl1 Union $ By <$> xs

toFacts :: Eq (Fact a) => Constraint a -> [Fact a]
toFacts Universe        = []
toFacts (By f)          = [f]
toFacts (Union lhs rhs) = nub $ toFacts lhs ++ toFacts rhs

getType :: IsConstraint -> NaiveType
getType (IsIndex _)      = Index
getType (IsString _)     = String
getType (IsFunction _ _) = Function

isOpen :: Constraint a -> Bool
isOpen Universe        = True
isOpen (Union lhs rhs) = isOpen lhs || isOpen rhs
isOpen _               = False

partitionFacts :: Constraint 'Index -> ([Constraint 'String], [Constraint 'String])
partitionFacts v = partitionEithers $ toFacts v <&> \case
  HasKey a    -> Right a
  HasNotKey a -> Left a

tryUnifyIndexConstraints :: ConstraintErrorM m => Constraint 'Index -> Constraint 'Index -> m (Constraint 'Index)
tryUnifyIndexConstraints lhs rhs
  | not $ null allConflicts = throwError $ Conflicts allConflicts
  | isOpen lhs              = return $ Union lhs rhs
  | null unsatisfied        = return lhs
  | otherwise               = throwError $ NotSatisfied unsatisfied
  where (lhsHasNot,lhsHas) = partitionFacts lhs
        (rhsHasNot,rhsHas) = partitionFacts rhs

        unsatisfied = do
            x <- rhsHas
            guard $ x `notElem` lhsHas
            return $ HasKey x

        allConflicts = nub $
          (rhsHasNot `conflicts` lhsHas) ++ (lhsHasNot `conflicts` rhsHas)
        conflicts xs ys = do
          x <- xs `intersect` ys
          return (HasKey x, HasNotKey x)

unifyConstraints :: ConstraintErrorM m => IsConstraint -> IsConstraint -> m IsConstraint
unifyConstraints (IsIndex lhs) (IsIndex rhs) = IsIndex <$> tryUnifyIndexConstraints lhs rhs
unifyConstraints (IsString Universe) rhs@(IsString _) = return rhs
unifyConstraints lhs@(IsString _) (IsString Universe) = return lhs
unifyConstraints lhs@(IsString (By lhsLiteral)) (IsString (By rhsLiteral))
  | rhsLiteral == lhsLiteral = return lhs
  | otherwise                = throwError $ Conflicts [(lhsLiteral,rhsLiteral)]
unifyConstraints lhs rhs = throwError $ UnexpectedType (getType lhs) (getType rhs)
