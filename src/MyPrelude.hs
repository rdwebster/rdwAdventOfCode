module MyPrelude
  (
    module Protolude
  -- , module Flow
  , module Monoid
  , module Control.Exception.Safe

  -- * Backwards Compatible Exports
  , Prelude.String
  , (Prelude.!!)
  , Prelude.getChar

  -- ** functions that work only on lists
  , showStr

  -- ** non-total functions
  , unsafeFromJust
  , unsafeHead
  , unsafeLast
  , unsafeRead
  , unsafeThrow

  -- ** Other functions
  , forever
  , forConcurrently
  , forConcurrently_
  , mapConcurrently_
  , mapConcurrently

  -- ** Testing and debugging
  , randomRIO
  )
  where

import Protolude hiding (
  (<>), maybeToEither, forever
  , SomeException
  , LeftAssociative
  , RightAssociative
  , catch
  , catchJust
  , catches
  , handle
  , handleJust
  , try
  , tryJust
  , bracket
  , bracket_
  , bracketOnError
  , onException
  , finally
  , throwIO
  , throwTo
  , moduleName
  , replace
  , minimumMay
  , option
  , trace
  )

import Control.Exception.Safe (
    MonadCatch
  , SomeException (..)
  , SomeAsyncException
  , catch
  , catchJust
  , catches
  , handle
  , handleJust
  , try
  , tryJust
  , bracket
  , bracket_
  , bracketOnError
  , onException
  , finally
  , throw
  , throwIO
  , throwTo
  , throwString
  )

import Control.Concurrent.Async (
    forConcurrently
  , forConcurrently_
  , mapConcurrently_
  , mapConcurrently
  )

import Data.Monoid as Monoid ((<>))
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Prelude
-- import Flow
import System.Random (randomRIO)

unsafeFromJust :: Maybe a -> a
unsafeFromJust = Maybe.fromJust

unsafeHead :: [a] -> a
unsafeHead = List.head

unsafeLast :: [a] -> a
unsafeLast = List.last

unsafeRead :: Read a => Prelude.String -> a
unsafeRead = Prelude.read

unsafeThrow :: Exception e => e -> ()
unsafeThrow = Exception.throw

showStr :: Show s => s -> Prelude.String
showStr = Prelude.show

-- Use custom forever due to known space leak.
-- see: https://ghc.haskell.org/trac/ghc/ticket/12804
-- TODO: Once Stack lts updates to transformers-0.5.5.0
-- which specializes the `*>` function for the StateT
-- Applicative instance, it should be safe to use the
-- new forever implemented in terms of Applicative.
forever :: (Monad m) => m a -> m b
forever a = let a' = a >> a' in a'
