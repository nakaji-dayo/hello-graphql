module DataSource (
  connect, defineEntity
  , connectWithLoadConfig
  , createPool'
  , Pool
  , withResource
  , destroyAllResources
  , Connection
  ) where

import           Data.Pool
import           Database.HDBC                   (disconnect)
-- import           Database.HDBC.PostgreSQL        (Connection, connectPostgreSQL)
import           Data.Default.Class
import           Database.HDBC.PostgreSQL.Pure   as PP (Address (..),
                                                        Config (..), Connection,
                                                        connect)
import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Schema.Driver     (Driver (getFieldsWithMap),
                                                  typeMap)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           EntityId
import           Language.Haskell.TH             (Dec, Name, Q, TypeQ)



createPool' :: IO (Pool Connection)
createPool' = createPool connect' disconnect 1 (realToFrac 60) 20

connect' :: IO Connection
connect' =
  connect $ def
  { user= "api"
  , password = "passwd"
  , database= "hello-graphql"
  , address =  AddressNotResolved "localhost" "5432"
  }

connectWithLoadConfig :: IO Connection
connectWithLoadConfig =
  connect'

-- defineEntity :: String -> [Name] -> Q [Dec]
-- defineEntity =
--   defineTableFromDB
--     connectWithLoadConfig
--     (driverPostgreSQL { typeMap = convTypes })
--     "public"

defineEntity :: String -> [Name] -> Q [Dec]
defineEntity =
  let driver = driverPostgreSQL { typeMap = convTypes }
      getFields m c l scm tbl = do
        (tm, nn) <- getFieldsWithMap driver m c l scm tbl
        return (map (getField' tbl) tm, nn)
  in defineTableFromDB
     connectWithLoadConfig
     (driver {getFieldsWithMap = getFields})
     "public"

convTypes :: [(String, TypeQ)]
convTypes = []
