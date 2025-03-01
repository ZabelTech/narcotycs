module Naive.Checker (checkCode
                     ,Constraint(..)
                     ,ConstraintType(..)
                     ,CheckerError(..)) where

import Control.Monad (forM_,void)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Lazy (MonadState(..),StateT,execStateT,gets,modify)
import qualified Data.Map as M

import Naive.Constraints
import Naive.Interface (builtInFunctions)
import Naive.Parser (Expr(..),Stmt(..))

type ConstraintMap = M.Map String IsConstraint

type CheckerErrorM m = MonadError CheckerError m
type CheckerStateM m = MonadState ConstraintMap m
type CheckerM m      = (CheckerErrorM m, CheckerStateM m)

data CheckerError = Constraint ConstraintError
                  | UnknownIdentifier String
                  | WrongArity {expected :: Int, actual :: Int }
  deriving (Eq,Show)

withCheckerError :: CheckerM m => Either ConstraintError a -> m a
withCheckerError = either (throwError . Constraint) return

runCheckerM :: CheckerErrorM m => StateT ConstraintMap m a -> m ConstraintMap
runCheckerM = flip execStateT $ M.fromList builtInFunctions

getConstraint :: CheckerM m => String -> m IsConstraint
getConstraint name = do
  ret <- gets $ M.lookup name
  maybe (throwError $ UnknownIdentifier name) return ret

setConstraint :: CheckerStateM m => String -> IsConstraint -> m ()
setConstraint name newConstraint = modify $ M.insert name newConstraint

applyExpression :: CheckerM m => [IsConstraint] -> (IsConstraint,ConstraintExpression) -> m ()
applyExpression argConstraints (arg,Naive.Constraints.Expression name expression) = do
  constraint <- withCheckerError $ expression argConstraints >>= combineConstraints arg
  setConstraint name constraint

collectConstraints :: CheckerErrorM m => [Stmt] -> m (M.Map String IsConstraint)
collectConstraints statements = runCheckerM $ do
  forM_ statements $ \case
    Assignment name expr         -> collectConstraint' expr >>= setConstraint name
    Naive.Parser.Expression expr -> void $ collectConstraint' expr
  where
    collectConstraint' :: CheckerM m => Expr -> m IsConstraint
    collectConstraint' (Variable name)     = getConstraint name
    collectConstraint' (StringLiteral lit) = return $ IsString $ By $ IsLiteral lit
    collectConstraint' (DictLiteral kvs)  = return $ fromKeys $ fst <$> kvs
    collectConstraint' (Application fnName args) = do
      getConstraint fnName >>= \case
        IsFunction argConstraints _ | length argConstraints /= length args ->
          throwError $ WrongArity (length argConstraints) (length args)
        IsFunction argConstraints retConstraint -> do
          collectedArgConstraints <- mapM collectConstraint' args
          forM_ (zip collectedArgConstraints argConstraints) $
            applyExpression collectedArgConstraints
          withCheckerError $
            expression retConstraint collectedArgConstraints
        x -> throwError $ Constraint $ UnexpectedType Function $ getType x
    fromKeys [] = IsIndex $ By $ HasNotKey Universe
    fromKeys ks = IsIndex $ fromFacts $ map (HasKey . By . IsLiteral) ks

checkCode :: [Stmt] -> Maybe CheckerError
checkCode = either return (const Nothing) . collectConstraints
