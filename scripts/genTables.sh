#!/bin/bash
set -eu

export PGPASSWORD=passwd

DB=${POSTGRES_DATABASE:-hello-graphql}

entityImport=./src/Entity.hs
entityImportDummies=./dummies.tmp
rm -f $entityImportDummies
touch $entityImportDummies

mkdir -p ./src/Entity
echo "module Entity" > $entityImport

tail='f'

rm -f ./src/Entity/*

tmpFactories=./factories.tmp
rm -f $tmpFactories
touch $tmpFactories
cat > ./test/Factories.hs <<EOF
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Factories where

import Factory
import Entity
import App
import EntityId
import Type
import Data.Default.Class
import Control.Monad
EOF

psql -h localhost -d ${DB} -U api -Atc "select tablename from pg_tables where schemaname='public' order by tablename" | \
    while read tbl; do
        entity=`echo $tbl | perl -pe "s/_(.)/\u\1/g; s/^(.)/\u\1/g"`
        lentity=`echo $entity | perl -ne 'print lcfirst'`
        if [ $entity != "SchemaMigrations" ]; then
            cat > ./src/Entity/${entity}.hs << EOS
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Entity.${entity} where

import           DataSource   (defineEntity)
import           Prelude      hiding (id)
import           GHC.Generics
import           Data.Typeable
import           Data.Generics.Labels ()

\$(defineEntity "${tbl}" [''Show, ''Generic, ''Eq, ''Typeable])

EOS

            echo -n "  " >> $entityImport
            if [ $tail == 't' ]; then
                echo -n ", " >> $entityImport
            else
                echo -n "( " >> $entityImport
            fi
            tail='t'

            printf "module Entity.${entity}\n" >> $entityImport
            printf "import Entity.${entity} (${entity} (${entity}), ${lentity})\n" >> $entityImportDummies
            printf "import qualified Entity.${entity} ()\n" >> $entityImportDummies

            cat >> ./test/Factories.hs << EOS
import Entity.${entity}
EOS
            cat >> ${tmpFactories} << EOS
instance Factory ${entity} where
  defFactory _ i = def & #id .~ ${entity}Id i
  persist = void . insertM insert${entity}
EOS
        fi
    done

printf "  ) where\n\n" >> $entityImport

cat $entityImportDummies >> $entityImport
rm $entityImportDummies

cat $tmpFactories >> ./test/Factories.hs
rm $tmpFactories

# entry ids

entityId=./src/EntityId.hs

cat > $entityId << EOS
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
module EntityId where

import           Data.Default.Class
import           Database.HDBC                        (SqlValue)
import           Database.HDBC.Schema.Driver          ()
import           Database.HDBC.Schema.PostgreSQL      ()
import           Database.Record
import           Database.Record.Persistable          ()
import           Database.Relational.ProjectableClass
import           GHC.Generics
import           GHC.Int
import           Language.Haskell.TH             (TypeQ)
import           Data.Proxy
import           Data.Morpheus.Types                  (DecodeScalar (..),
                                                       EncodeScalar (..),
                                                       GQLType (..))
import           Data.Text                            (Text, pack)
import           Data.Morpheus.Kind                   (SCALAR)

instance Default Text where
  def = pack ""

EOS


##
### PK
query=$(cat << EOS
SELECT
    tc.table_name,
    kcu.column_name
FROM
    information_schema.table_constraints AS tc
    JOIN information_schema.key_column_usage AS kcu
      ON tc.constraint_name = kcu.constraint_name
      AND tc.table_schema = kcu.table_schema
WHERE tc.constraint_type = 'PRIMARY KEY' AND tc.table_name <> 'schema_migrations'
order by tc.table_name, kcu.column_name;
EOS
)
IFS='|'
psql -h localhost -d ${DB} -U api -Atc "$query" \
    | while read tbl clm; do
        entity=`echo $tbl | perl -pe "s/_(.)/\u\1/g; s/^(.)/\u\1/g"`
        cat >> $entityId << EOS
newtype ${entity}Id = ${entity}Id { un${entity}Id :: Text }
  deriving (Generic, Eq, Show, Ord, Default, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue
           , HasColumnConstraint NotNull, EncodeScalar, DecodeScalar)
instance GQLType ${entity}Id where
  type KIND ${entity}Id = SCALAR

EOS

done

echo "getField' :: String -> (String, TypeQ) -> (String, TypeQ)" >> $entityId
psql -h localhost -d ${DB} -U api -Atc "$query" \
    | while read tbl clm; do
        entity=`echo $tbl | perl -pe "s/_(.)/\u\1/g; s/^(.)/\u\1/g"`
        echo "getField' \"${tbl}\" (c@\"${clm}\", _) = (c, [t|${entity}Id|])" >> $entityId
    done

### FK
query=$(cat << EOS
SELECT
    tc.table_name,
    kcu.column_name,
    ccu.table_name AS foreign_table_name,
    ccu.column_name AS foreign_column_name,
    c.is_nullable
FROM
    information_schema.table_constraints AS tc
    JOIN information_schema.key_column_usage AS kcu
      ON tc.constraint_name = kcu.constraint_name
      AND tc.table_schema = kcu.table_schema
    JOIN information_schema.constraint_column_usage AS ccu
      ON ccu.constraint_name = tc.constraint_name
      AND ccu.table_schema = tc.table_schema
    JOIN INFORMATION_SCHEMA.COLUMNS as c
      ON tc.table_name = c.table_name AND kcu.column_name = c.column_name
WHERE tc.constraint_type = 'FOREIGN KEY'
order by tc.table_name, kcu.column_name, kcu.column_name, ccu.column_name;
EOS
     )
psql -h localhost -d ${DB} -U api -Atc "$query" \
    | while read tbl clm ftbl fclm nullable ; do
    fentity=`echo $ftbl | perl -pe "s/_(.)/\u\1/g; s/^(.)/\u\1/g"`
    [ $nullable == 'YES' ] && maybe='Maybe ' || maybe=''
    echo "getField' \"${tbl}\" (c@\"${clm}\", _) = (c, [t|${maybe}${fentity}Id|])" >> $entityId
done
### fallback
echo "getField' _ x = x" >> $entityId
