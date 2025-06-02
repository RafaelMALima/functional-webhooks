{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty
import Control.Monad.IO.Class        (liftIO)
import Data.ByteString.Lazy.Char8 as B
import Data.Aeson                    
import Data.Aeson.Types              (Parser)
import GHC.Generics                  (Generic)
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Network.Curl
import Text.Read                     (readMaybe)
import qualified Data.Text as T      
import qualified Data.Text.Read as TR
import qualified Data.Text.Lazy as TL

data Transaction = Transaction
      {event:: !String
      ,transaction_id:: !String
      ,amount:: !String
      ,currency:: !String 
      ,timestamp:: !String} deriving (Show, Eq, Generic)


instance FromJSON Transaction
instance ToJSON   Transaction

data MaybeTransaction = MaybeTransaction
      {mEvent:: Maybe String
      ,mTransaction_id:: !String
      ,mAmount:: Maybe String
      ,mCurrency:: Maybe String 
      ,mTimestamp:: Maybe String} deriving (Show, Eq, Generic)

instance FromJSON MaybeTransaction where
  parseJSON = withObject "MaybeTransaction" $ \o -> MaybeTransaction
        <$> o .:? "event"            -- field 1: mEvent
        <*> o .:  "transaction_id"   -- field 2: mTransaction_id
        <*> o .:? "amount"           -- field 3: mAmount
        <*> o .:? "currency"         -- field 4: mCurrency
        <*> o .:? "timestamp"        -- field 5: mTimestamp


instance ToRow Transaction where
  toRow (Transaction e tid amt cur ts) =
    toRow (e, tid, amt, cur, ts)

writeToDatabase :: Connection -> Transaction -> IO ()
writeToDatabase conn rt = execute 
                            conn
                            "INSERT OR REPLACE INTO transactions \
                            \(event, transaction_id, amount, currency, timestamp) \
                            \VALUES (?,?,?,?,?)"
                            rt


countExisting :: Connection -> Transaction -> IO Int
countExisting conn rt = do
  [Only n] <- query conn
                "SELECT COUNT(*)                      \
                \FROM transactions                     \
                \WHERE transaction_id = ?"
                (Only (transaction_id rt))
  pure n          -- ^ n :: Int, so the whole action is IO Int

prepareOpts :: String -> TL.Text -> [CurlOption]
prepareOpts j t =
  [ CurlPost True
  , CurlPostFields [j]
  , CurlHttpHeaders
      [ "Content-Type: application/json"
      , "X-Webhook-Token: " ++ TL.unpack t
      ]
  ]

prepareOptsNoToken :: String -> [CurlOption]
prepareOptsNoToken j = 
  [ CurlPost True
  , CurlPostFields [j]
  , CurlHttpHeaders
      [ "Content-Type: application/json" ]
  ]

curlRequest :: String -> [CurlOption] -> IO ()
curlRequest url opts = do (code, body) <- curlGetString url opts
                          print $ "libcurl finished with code: " ++ show code
                          print "---------- response body ----------"
                          print body

data TxError
  = MissingField T.Text
  deriving (Show, Eq)

-- | Promote a 'MaybeTransaction' to a fully-checked 'Transaction'.
toTransaction :: MaybeTransaction -> Either TxError Transaction
toTransaction (MaybeTransaction ev tid am cur ts) = do
  event     <- req "event"     ev
  amount    <- req "amount"    am
  currency  <- req "currency"  cur
  timestamp <- req "timestamp" ts
  pure $ Transaction event tid amount currency timestamp
  where
    req name = maybe (Left $ MissingField name) Right
 
main :: IO ()
main = do  
    conn <- open "test.db"
    execute_
      conn
      "DROP TABLE IF EXISTS transactions"

    execute_
      conn
      "CREATE TABLE IF NOT EXISTS transactions \
      \( event         TEXT \
      \, transaction_id TEXT PRIMARY KEY \
      \, amount         REAL \
      \, currency       TEXT \
      \, timestamp      TEXT \
      \)"

    scotty 5000 $ do
      get "/:word" $ do
          beam <- pathParam "word"
          html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
      post "/webhook" $ do
          b <- body
          mrt <- jsonData :: ActionM MaybeTransaction
          let (.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
              (.:) = (.) . (.)
          let makeRequest = (liftIO . withCurlDo) .: curlRequest

          case toTransaction mrt of
            Left (MissingField fld)-> do
              let url = "http://localhost:5001/cancelar"
              let opts = prepareOptsNoToken (B.unpack b)
              makeRequest url opts
              status status401
              let msg = TL.fromStrict $ T.pack (mTransaction_id mrt) <> " missing field: " <> fld
              text  msg
              finish
            Right rt -> do
              mContent <- header "Content-Type"
              mToken   <- header "X-Webhook-Token"   
              liftIO $ B.putStrLn (encode rt)               
              -- liftIO $ print mContent
              -- liftIO $ print mToken

              let tj = B.unpack $ encode rt

              n <- liftIO $ countExisting conn rt
              liftIO $ Prelude.putStrLn $ "duplicates = " ++ show n
              if n == 0
                    then  status status201        
                    else do 
                      let opts = prepareOptsNoToken tj 
                      let url = "http://localhost:5001/cancelar"
                      makeRequest url opts
                      status status401
                      json rt
                      finish
              
              let amtText = T.pack (amount rt)                -- String to Text to Maybe Double 
                  rm :: Maybe Double
                  rm = case TR.double amtText of
                         Right (d, _) -> Just d
                         Left  _      -> Nothing

              -- liftIO $ print rm
              let write :: Bool = case rm of
                                    Just a -> (a > 0.0) 
                                    Nothing -> False

              if write 
                then liftIO $ writeToDatabase conn rt 
                else do
                  let opts = prepareOptsNoToken tj 
                  let url = "http://localhost:5001/cancelar" 
                  makeRequest url opts
                  status status401
                  json rt
                  finish

              let url = "http://localhost:5001/confirmar"   -- matches the FastAPI server
              case mToken of
                Nothing   -> do
                  let opts = prepareOptsNoToken tj 
                  makeRequest url opts
                  status status401
                  status status200
                  json rt

                Just tok  -> do
                  let opts = prepareOpts tj tok
                  makeRequest url opts
                  status status200
                  json rt
