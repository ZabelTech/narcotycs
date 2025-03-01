{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Naive.Interface where

import Control.Monad.Except (MonadError(..))
import Naive.Constraints

builtInFunctions :: [(String,IsConstraint)]
builtInFunctions = [
  ("get",IsFunction [
      By $ Expression "index" $ \(_:IsString key:_) -> return $ IsIndex $ fromFacts [HasKey key],
      By $ Expression "key" $ \_args -> return $ IsString Universe
      ] $ By $ Expression "return" $ \_args -> return $ IsString Universe
  ),
  ("add",IsFunction [
      By $ Expression "index" $ \(_:IsString key:_) -> return $ IsIndex $ fromFacts [HasNotKey key],
      By $ Expression "key" $ \_args -> return $ IsString Universe,
      By $ Expression "value" $ \_args -> return $ IsString Universe
      ] $ By $ Expression "return" $ \(tbl:IsString key:_) -> unionWithFact tbl $ HasKey key
  ),
  ("del",IsFunction [
      By $ Expression "index" $ \(_:IsString key:_) -> return $ IsIndex $ fromFacts [HasKey key],
      By $ Expression "key" $ \_args -> return $ IsString Universe
      ] $ By $ Expression "return" $ \(tbl:IsString key:_) -> unionWithFact tbl $ HasNotKey key
  )
  ]
  where unionWithFact (IsIndex ks) = return . IsIndex . Union ks . By
        unionWithFact x = const $ throwError $ UnexpectedType Index $ getType x
