{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.TH.EntityHaddockSpec (spec) where

import TemplateTestImports

import Database.Persist.TH.CommentSpec (CommentModel (..))
import Language.Haskell.TH (DocLoc (DeclDoc), getDoc, runQ)

spec :: Spec
spec = describe "EntityHaddockSpec" $ do
    it "generates entity Haddock" $ do
        maybeDoc <- runQ $ getDoc (DeclDoc 'CommentModel)
        maybeDoc `shouldBe` Just "Doc comments work.\nHas multiple lines."
