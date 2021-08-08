{-| Generate CSV & XLSX files to use with CoinTracking's import feature.

-}
module Web.CoinTracking.Imports
    ( -- * Types
      module Web.CoinTracking.Imports.Types
      -- * CSV Import Files
    , coinTrackingCsvImport
    , headerRow
    , csvEncodingOptions
    ) where

import           Data.Csv                       ( EncodeOptions(..)
                                                , defaultEncodeOptions
                                                , encodeWith
                                                )

import           Web.CoinTracking.Imports.Types

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


-- CSVs

-- | Generate the CoinTracking CSV Import for the data, prepended by
-- a header row.
coinTrackingCsvImport :: [CTImportData] -> LBS.ByteString
coinTrackingCsvImport = (headerRow <>) . encodeWith csvEncodingOptions

-- | The CSV header row to prepend to the generated output.
headerRow :: LBS.ByteString
headerRow = encodeWith
    csvEncodingOptions
    [ [ "Type" :: T.Text
      , "Buy"
      , "Cur."
      , "Sell"
      , "Cur."
      , "Fee"
      , "Cur."
      , "Exchange"
      , "Trade-Group"
      , "Comment"
      , "Date"
      , "Tx-ID"
      , "Buy Value in your Account Currency"
      , "Sell Value in your Account Currency"
      ]
    ]

-- | 'defaultEncodeOptions', but with newline-only line endings.
csvEncodingOptions :: EncodeOptions
csvEncodingOptions = defaultEncodeOptions { encUseCrLf = False }
