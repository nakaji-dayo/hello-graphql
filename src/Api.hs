{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Api where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocument)
import           Data.Morpheus.Types        (RootResolver (..), Undefined (..))
import           Data.Text                  (Text)

importGQLDocument "schema.graphql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = deityR},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
deityR DeityArgs {name} =
  pure Deity
  { name = pure name,
    power = pure (Just "Shapeshifting")
  }

api :: ByteString -> IO ByteString
api = interpreter rootResolver
