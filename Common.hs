{-# LANGUAGE FlexibleContexts #-}

module Common where

import Bnfc.Abs (BNFC'Position)
import Control.Monad.Except (MonadError (throwError))

type Err = Either String

throwErrorPos :: (MonadError String m) => BNFC'Position -> String -> m a
throwErrorPos Nothing e = throwError e
throwErrorPos (Just (l, _)) e = throwError (e ++ " at line " ++ show l)