{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Queue where

import Database.SqlServer.Types.Identifiers

import Test.QuickCheck
import Data.DeriveTH
import Data.Word (Word16)
import Text.PrettyPrint
import Data.Maybe (isJust)

-- TODO username support
data ExecuteAs = Self
               | Owner

-- TODO procedure support
data Activation = Activation
    {
      maxQueueReaders  :: Maybe Word16
    , executeAs :: Maybe ExecuteAs
    } 

data QueueDefinition = QueueDefinition
    {
      queueName :: RegularIdentifier
    , queueStatus    :: Maybe Bool
    , retention :: Maybe Bool
    , activation :: Maybe Activation
    , poisonMessageHandling :: Maybe Bool
    }

derive makeArbitrary ''ExecuteAs
derive makeArbitrary ''Activation
derive makeArbitrary ''QueueDefinition

anySpecified :: QueueDefinition -> Bool
anySpecified q = isJust (queueStatus q) || isJust (retention q) ||
                 isJust (activation q)  || isJust (poisonMessageHandling q)

anyActivationValues :: Activation -> Bool
anyActivationValues a = isJust (maxQueueReaders a) || isJust (executeAs a)

renderStatus :: Bool -> Doc
renderStatus True  = text "STATUS = ON"
renderStatus False = text "STATUS = OFF"

renderRetention :: Bool -> Doc
renderRetention True  = text "RETENTION = ON"
renderRetention False = text "RETENTION = OFF"

renderPoisonMessageHandling :: Bool -> Doc
renderPoisonMessageHandling True = text "POISON_MESSAGE_HANDLING(STATUS = ON)"
renderPoisonMessageHandling False = text "POISON_MESSAGE_HANDLING(STATUS = OFF)"

renderMaxQueueReaders :: Word16 -> Doc
renderMaxQueueReaders t = text "MAX_QUEUE_READERS = " <> int (fromIntegral t)

renderExecuteAs :: ExecuteAs -> Doc
renderExecuteAs Self = text "EXECUTE AS SELF"
renderExecuteAs Owner = text "EXECUTE AS OWNER"

renderActivation :: Activation -> Doc
renderActivation a
  | anyActivationValues a = text "ACTIVATION(" <+>
                            hcat (punctuate comma $ filter (/= empty)
                                  [ maybe empty renderMaxQueueReaders (maxQueueReaders a) 
                                  , maybe empty renderExecuteAs (executeAs a)
                                  ]) <+> text ")"
  | otherwise             = empty
                            
renderQueue :: QueueDefinition -> Doc
renderQueue q = text "CREATE QUEUE" <+> (renderRegularIdentifier (queueName q)) <+>
                options
  where
    options
      | not $ anySpecified q = empty
      | otherwise       = text "WITH" <+>
                          hcat (punctuate comma $ filter (/= empty)
                             [ maybe empty renderStatus (queueStatus q) 
                             , maybe empty renderRetention (retention q)
                             , maybe empty renderActivation (activation q)
                             , maybe empty renderPoisonMessageHandling (poisonMessageHandling q)])
