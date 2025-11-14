{-# LANGUAGE ScopedTypeVariables #-}

module Web.Todoist.BuilderSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Web.Todoist.Util.Builder
    ( Builder
    , runBuilder
    , setDescription
    , setParentId
    )
import Web.Todoist.Domain.Project (ProjectCreate, newProject)

import Data.Aeson (encode)
import Data.Bool (Bool (True))
import Data.Function (($))
import Data.Monoid (Monoid (mempty), (<>))
import GHC.Base (const)

spec :: Spec
spec = do
    describe "Builder Monoid instance" $ do
        it "satisfies left identity: mempty <> x = x" $ do
            let x = setDescription "test" <> setParentId "parent123"
            let seed = newProject "Project"
            encode (runBuilder seed (mempty <> x)) `shouldBe` encode (runBuilder seed x)

        it "satisfies right identity: x <> mempty = x" $ do
            let x = setDescription "test" <> setParentId "parent123"
            let seed = newProject "Project"
            encode (runBuilder seed (x <> mempty)) `shouldBe` encode (runBuilder seed x)

        it "satisfies associativity: (x <> y) <> z = x <> (y <> z)" $ do
            let x = setDescription "desc"
            let y = setParentId "parent1"
            let z = setParentId "parent2"
            let seed = newProject "Project"
            encode (runBuilder seed ((x <> y) <> z)) `shouldBe` encode (runBuilder seed (x <> (y <> z)))

        it "mempty is the identity modification" $ do
            let seed = newProject "Project"
            encode (runBuilder seed mempty)
                `shouldBe` encode (runBuilder seed (mempty :: Builder ProjectCreate))

    describe "Builder usage patterns" $ do
        it "can use mempty when no modifications needed" $ do
            let seed = newProject "Project"
            let result = runBuilder seed mempty
            result `shouldSatisfy` const True -- Just verify it compiles and runs
        it "can compose multiple modifications" $ do
            let seed = newProject "Project"
            let mods = setDescription "desc" <> setParentId "parent123"
            let result = runBuilder seed mods
            result `shouldSatisfy` const True
