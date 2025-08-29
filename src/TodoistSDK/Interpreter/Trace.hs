{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module TodoistSDK.Interpreter.Trace (
  Op(..),
  Trace(..)
) where

import TodoistSDK.Capabilities
import TodoistSDK.Types ( ProjectId )  

import Control.Monad.Trans.Writer

data Op = GetAllProjects deriving (Show)

newtype Trace a = Trace { runTrace :: Writer [Op] a }
  deriving (Functor, Applicative, Monad)

instance TodoistProjectM Trace where
  getAllProjects :: Trace [ProjectId]
  getAllProjects = Trace $ do
    tell [GetAllProjects]
    pure []