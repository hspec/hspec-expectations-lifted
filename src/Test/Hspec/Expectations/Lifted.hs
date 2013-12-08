-- |
-- Introductory documentation: <https://github.com/sol/hspec-expectations#readme>
module Test.Hspec.Expectations.Lifted (

-- * Setting expectations
  expectationFailure
, shouldBe
, shouldSatisfy
, shouldContain
, shouldMatchList
, shouldReturn
) where

import           Control.Monad.IO.Class
import qualified Test.Hspec.Expectations as E

infix 1 `shouldBe`, `shouldSatisfy`, `shouldContain`, `shouldMatchList`, `shouldReturn`

liftIO2 :: MonadIO m => (a -> b -> IO r) -> a -> b -> m r
liftIO2 action a = liftIO . action a

-- | This is just an alias for HUnit's `assertFailure`.
expectationFailure :: MonadIO m => String -> m ()
expectationFailure = liftIO . E.expectationFailure

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@ (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe = liftIO2 E.shouldBe

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (Show a, MonadIO m) => a -> (a -> Bool) -> m ()
shouldSatisfy = liftIO2 E.shouldSatisfy

-- |
-- @list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained,
-- wholly and intact, anywhere in the second.
shouldContain :: (Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldContain = liftIO2 E.shouldContain

-- |
-- @xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same
-- elements that @ys@ has, possibly in another order
shouldMatchList :: (Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldMatchList = liftIO2 E.shouldMatchList

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
shouldReturn :: (Show a, Eq a, MonadIO m) => m a -> a -> m ()
action `shouldReturn` expected = action >>= liftIO . (`E.shouldBe` expected)
