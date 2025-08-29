{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Web.Todoist.Runner.Trace (
  Op(..),
  Trace(..)
) where

import Web.Todoist.Project

import Control.Monad.Trans.Writer

data Op = GetAllProjects deriving (Show)

newtype Trace a = Trace { runTrace :: Writer [Op] a }
  deriving (Functor, Applicative, Monad)

instance TodoistProjectM Trace where
  getAllProjects :: Trace [ProjectId]
  getAllProjects = Trace $ do
    tell [GetAllProjects]
    pure []