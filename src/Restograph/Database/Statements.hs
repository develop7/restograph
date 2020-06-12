{-# LANGUAGE QuasiQuotes #-}

module Restograph.Database.Statements where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, rowsAffectedStatement, vectorStatement)

createNode :: Statement Text (Maybe Int32)
createNode =
  [maybeStatement|
    INSERT INTO "nodes" ("label") VALUES ($1 :: text) RETURNING "id" :: int4
    |]

deleteNode :: Statement Int32 Int64
deleteNode =
  [rowsAffectedStatement|
    DELETE FROM "nodes" WHERE id = $1 :: int4
    |]

renameNode :: Statement (Int32, Text) Int64
renameNode =
  [rowsAffectedStatement|
    UPDATE "nodes" SET "label" = $2 :: TEXT WHERE "id" = $1 :: int4
    |]

linkNodes :: Statement (Int32, Int32) Int64
linkNodes =
  [rowsAffectedStatement|
    INSERT INTO "links"
    VALUES (greatest($1 :: INT4, $2 :: INT4), least($1 :: INT4, $2 :: INT4))
    |]

listNodes :: Statement () (Vector (Int32, Text))
listNodes =
  -- ordering IDs to match the order of unique index
  [vectorStatement|
    SELECT "id" :: int4, "label" :: text FROM "nodes"
    |]

listNodeNeighbors :: Statement Int32 (Vector (Int32, Text))
listNodeNeighbors =
  [vectorStatement|
    SELECT "id" :: INT4, label :: TEXT
    FROM "nodes"
    WHERE "id" IN (SELECT "id_right"
                 FROM "links"
                 WHERE "id_left" = $1 :: INT4
                 UNION
                 SELECT "id_left"
                 FROM "links"
                 WHERE "id_right" = $1 :: INT4)
    |]
