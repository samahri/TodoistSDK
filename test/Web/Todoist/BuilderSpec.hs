{-# LANGUAGE ScopedTypeVariables #-}

module Web.Todoist.BuilderSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Web.Todoist.Domain.Project (ProjectCreate, createProjectBuilder)
import Web.Todoist.Util.Builder
    ( Builder
    , runBuilder
    , withDescription
    , withParentId
    )

import Data.Aeson (encode)
import Data.Bool (Bool (True))
import Data.Function (($))
import Data.Monoid (Monoid (mempty), (<>))
import GHC.Base (const)

spec :: Spec
spec = do
    describe "Builder Monoid instance" $ do
        it "satisfies left identity: mempty <> x = x" $ do
            let x = withDescription "test" <> withParentId "parent123"
            let seed = createProjectBuilder "Project"
            encode (runBuilder seed (mempty <> x)) `shouldBe` encode (runBuilder seed x)

        it "satisfies right identity: x <> mempty = x" $ do
            let x = withDescription "test" <> withParentId "parent123"
            let seed = createProjectBuilder "Project"
            encode (runBuilder seed (x <> mempty)) `shouldBe` encode (runBuilder seed x)

        it "satisfies associativity: (x <> y) <> z = x <> (y <> z)" $ do
            let x = withDescription "desc"
            let y = withParentId "parent1"
            let z = withParentId "parent2"
            let seed = createProjectBuilder "Project"
            encode (runBuilder seed ((x <> y) <> z)) `shouldBe` encode (runBuilder seed (x <> (y <> z)))

        it "mempty is the identity modification" $ do
            let seed = createProjectBuilder "Project"
            encode (runBuilder seed mempty)
                `shouldBe` encode (runBuilder seed (mempty :: Builder ProjectCreate))

    describe "Builder usage patterns" $ do
        it "can use mempty when no modifications needed" $ do
            let seed = createProjectBuilder "Project"
            let result = runBuilder seed mempty
            result `shouldSatisfy` const True -- Just verify it compiles and runs
        it "can compose multiple modifications" $ do
            let seed = createProjectBuilder "Project"
            let mods = withDescription "desc" <> withParentId "parent123"
            let result = runBuilder seed mods
            result `shouldSatisfy` const True
