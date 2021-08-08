{-| Type definitions, instances, & utility functions for CoinTracking
imports.

-}
{-# LANGUAGE RecordWildCards #-}
module Web.CoinTracking.Imports.Types
    ( CTExportData(..)
    , CTTransactionType(..)
    , Amount(..)
    , Currency(..)
    ) where

import           Data.Csv                       ( Field
                                                , ToField(..)
                                                , ToRecord(..)
                                                , record
                                                )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , Scientific
                                                , formatScientific
                                                )
import           Data.Time                      ( ZonedTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           GHC.Generics                   ( Generic )

import qualified Data.Text                     as T

-- | Represents a single row in an export.
data CTExportData = CTExportData
    { ctedType      :: CTTransactionType
    , ctedBuy       :: Maybe Amount
    , ctedSell      :: Maybe Amount
    , ctedFee       :: Maybe Amount
    , ctedExchange  :: T.Text
    , ctedGroup     :: T.Text
    , ctedComment   :: T.Text
    , ctedDate      :: ZonedTime
    , ctedTradeId   :: T.Text
    , ctedBuyValue  :: Maybe Amount
    , ctedSellValue :: Maybe Amount
    }
    deriving (Show, Read, Generic)

instance ToRecord CTExportData where
    toRecord CTExportData {..} = record
        [ toField ctedType
        , orBlank renderAmount   ctedBuy
        , orBlank renderCurrency ctedBuy
        , orBlank renderAmount   ctedSell
        , orBlank renderCurrency ctedSell
        , orBlank renderAmount   ctedFee
        , orBlank renderCurrency ctedFee
        , toField ctedExchange
        , toField ctedGroup
        , toField ctedComment
        , toField $ formatTime defaultTimeLocale "%F %T" ctedDate
        , toField ctedTradeId
        , orBlank renderAmount ctedBuyValue
        , orBlank renderAmount ctedSellValue
        ]
      where
        orBlank :: (a -> Field) -> Maybe a -> Field
        orBlank = maybe ""
        renderAmount :: Amount -> Field
        renderAmount Amount { aCurrency = Currency {..}, ..} =
            toField $ formatScientific Fixed (Just cPrecision) aAmount
        renderCurrency :: Amount -> Field
        renderCurrency = toField . cTicker . aCurrency


-- | An amount & currency specification.
data Amount = Amount
    { aAmount   :: Scientific
    -- ^ The total amount.
    , aCurrency :: Currency
    -- ^ The currency symbol & decimal-precision.
    }
    deriving (Show, Read, Eq)

-- | A pair containing a currency symbol & an amount.
data Currency = Currency
    { cPrecision :: Int
    -- ^ The number of decimals places of precision.
    , cTicker    :: T.Text
    -- ^ The ticker symbol
    }
    deriving (Show, Read, Eq, Generic)


-- | Possible types for an imported transaction.
data CTTransactionType
    = Trade
    | Deposit
    | Withdrawal
    | Income
    | Mining
    | GiftTipIn
    | Spend
    | Donation
    | GiftOut
    | Stolen
    | Lost
    | Airdrop
    | Staking
    | Masternode
    | Minting
    | DividendsIncome
    | LendingIncome
    | InterestIncome
    | RewardBonus
    | MiningCommercial
    | MarginProfit
    | DerivativesFuturesProfit
    | OtherIncome
    | IncomeNonTaxable
    | OtherIncomeNonTaxable
    | MarginLoss
    | MarginFee
    | BorrowingFee
    | SettlementFee
    | DerivativesFuturesLoss
    | OtherFee
    | OtherExpense
    | ExpenseNonTaxable
    | MarginTrade
    | DerivativesFuturesTrade
    deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance ToField CTTransactionType where
    toField = \case
        Trade                    -> "Trade"
        Deposit                  -> "Deposit"
        Withdrawal               -> "Withdrawal"
        Income                   -> "Income"
        Mining                   -> "Mining"
        GiftTipIn                -> "Gift/Tip(In)"
        Spend                    -> "Spend"
        Donation                 -> "Donation"
        GiftOut                  -> "Gift(Out)"
        Stolen                   -> "Stolen"
        Lost                     -> "Lost"
        Airdrop                  -> "Airdrop"
        Staking                  -> "Staking"
        Masternode               -> "Masternode"
        Minting                  -> "Minting"
        DividendsIncome          -> "Dividends Income"
        LendingIncome            -> "Lending Income"
        InterestIncome           -> "Interest Income"
        RewardBonus              -> "Reward / Bonus"
        MiningCommercial         -> "Mining (commercial)"
        MarginProfit             -> "Margin Profit"
        DerivativesFuturesProfit -> "Derivatives / Futures Profit"
        OtherIncome              -> "Other Income"
        IncomeNonTaxable         -> "Income (non taxable)"
        OtherIncomeNonTaxable    -> "Other Income (non taxable)"
        MarginLoss               -> "Margin Loss"
        MarginFee                -> "Margin Fee"
        BorrowingFee             -> "Borrowing Fee"
        SettlementFee            -> "Settlement Fee"
        DerivativesFuturesLoss   -> "Derivatives / Futures Loss"
        OtherFee                 -> "Other Fee"
        OtherExpense             -> "Other Expense"
        ExpenseNonTaxable        -> "Expense (non taxable)"
        MarginTrade              -> "Margin Trade"
        DerivativesFuturesTrade  -> "Derivatives / Futures Trade"
