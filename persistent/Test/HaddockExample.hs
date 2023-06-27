{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.HaddockExample where

import Database.Persist.TH
import Language.Haskell.TH

mkPersist (sqlSettings {mpsEntityHaddocks = True}) [persistLowerCase|

-- | Comment on entity.
-- | Has multiple lines.
CommentModel
    -- | Comment on column.
    -- | Has multiple lines.
    comment1 String
    noComment Bool
    -- | Comment on column.
    comment2 Int

|]
