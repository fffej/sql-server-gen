{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Queue where

import Database.SqlServer.Types.Identifiers hiding (unwrap)
import Database.SqlServer.Types.Properties
import Database.SqlServer.Types.Procedure

import Test.QuickCheck
import Data.DeriveTH
import Data.Word (Word16)
import Text.PrettyPrint
import Data.Maybe (isJust)

-- TODO username support
data ExecuteAs = Self
               | Owner

-- Activation procedures can not have any parameters
newtype ZeroParamProc = ZeroParamProc { unwrap :: ProcedureDefinition }

instance Arbitrary ZeroParamProc where
  arbitrary = do
    proc <- arbitrary :: Gen ProcedureDefinition
    return $ ZeroParamProc (proc { parameters = [] })

data Activation = Activation
    {
      maxQueueReaders  ::  Word16
    , executeAs :: ExecuteAs
    , procedure :: ZeroParamProc
    } 

data QueueDefinition = QueueDefinition
    {
      queueName :: RegularIdentifier
    , queueStatus :: Maybe Bool
    , retention :: Maybe Bool
    , activation :: Maybe Activation
    , poisonMessageHandling :: Maybe Bool
    }

instance NamedEntity QueueDefinition where
  name = queueName

derive makeArbitrary ''ExecuteAs
derive makeArbitrary ''QueueDefinition

instance Arbitrary Activation where
  arbitrary = do
    r <- choose (0,32767)
    x <- arbitrary
    y <- arbitrary
    return $ Activation r x y

anySpecified :: QueueDefinition -> Bool
anySpecified q = isJust (queueStatus q) || isJust (retention q) ||
                 isJust (activation q)  || isJust (poisonMessageHandling q)

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

renderProc :: Activation -> Doc
renderProc a = renderProcedureDefinition (unwrap $ procedure a)

renderProcedureName :: ProcedureDefinition -> Doc
renderProcedureName a = text "PROCEDURE_NAME =" <+> renderRegularIdentifier (procedureName a)

renderActivation :: Activation -> Doc
renderActivation a = text "ACTIVATION(" <+>
                     hcat (punctuate comma $ filter (/= empty)
                           [ renderMaxQueueReaders (maxQueueReaders a) 
                           , renderExecuteAs (executeAs a)
                           , renderProcedureName (unwrap $ procedure a)
                           ]) <+> text ")"

renderQueueDefinition :: QueueDefinition -> Doc
renderQueueDefinition q =  maybe empty renderProc (activation q) $+$
                           text "CREATE QUEUE" <+> (renderRegularIdentifier (queueName q)) <+> options $+$ text "GO"
  where
    options
      | not $ anySpecified q = empty
      | otherwise       = text "WITH" <+>
                          hcat (punctuate comma $ filter (/= empty)
                             [ maybe empty renderStatus (queueStatus q) 
                             , maybe empty renderRetention (retention q)
                             , maybe empty renderActivation (activation q)
                             , maybe empty renderPoisonMessageHandling (poisonMessageHandling q)])

instance Show QueueDefinition where
  show = render . renderQueueDefinition
