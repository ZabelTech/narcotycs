{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Naive.Interface where

import Control.Monad.Except (MonadError(..))
import Naive.Constraints

builtInFunctions :: [(String,IsConstraint)]
builtInFunctions = [
  ("get",IsFunction [
      Expression "index" $ \(_:IsString key:_) -> return $ IsIndex $ fromFacts [HasKey key],
      Expression "key" $ \_args -> return $ IsString Universe
      ] $ Expression "return" $ \_args -> return $ IsString Universe
  ),
  ("add",IsFunction [
      Expression "index" $ \(_:IsString key:_) -> return $ IsIndex $ fromFacts [HasNotKey key],
      Expression "key" $ \_args -> return $ IsString Universe,
      Expression "value" $ \_args -> return $ IsString Universe
      ] $ Expression "return" $ \(tbl:IsString key:_) -> unionWithFact tbl $ HasKey key
  ),
  ("del",IsFunction [
      Expression "index" $ \(_:IsString key:_) -> return $ IsIndex $ fromFacts [HasKey key],
      Expression "key" $ \_args -> return $ IsString Universe
      ] $ Expression "return" $ \(tbl:IsString key:_) -> unionWithFact tbl $ HasNotKey key
  )
  ]
  where unionWithFact (IsIndex ks) = return . IsIndex . Union ks . By
        unionWithFact x = const $ throwError $ UnexpectedType Index $ getType x
