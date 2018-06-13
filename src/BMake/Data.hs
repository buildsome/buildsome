module BMake.Data
    ( MetaVar(..), metaVarChar
    , MetaVarModifier(..), metaVarModifierChar
    , metaVarString
    , Expr3(..) -- ToDo: rename
    ) where

import Prelude.Compat

--------------------------------------------------------------------------------
import           Control.DeepSeq          (NFData (..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Aeson
import           Data.Binary              (Binary)
import           Data.ByteString.Lazy     (ByteString)
import           GHC.Generics
--------------------------------------------------------------------------------

data MetaVar
    = FirstOutput
    | FirstInput
    | AllInputs
    | AllOOInputs
    | Stem
    deriving (Eq, Ord, Show, Generic)
instance ToJSON MetaVar where -- ToDo: remove when not needed
instance Binary MetaVar where -- ToDo: remove when not needed
instance NFData MetaVar where
    rnf = genericRnf

metaVarChar :: MetaVar -> Char
metaVarChar FirstOutput = '@'
metaVarChar FirstInput = '<'
metaVarChar AllInputs = '^'
metaVarChar AllOOInputs = '|'
metaVarChar Stem = '*'

data MetaVarModifier
    = NoMod
    | ModFile
    | ModDir
    deriving (Eq, Ord, Show, Generic)
instance ToJSON MetaVarModifier where -- ToDo: remove when not needed
instance Binary MetaVarModifier where -- ToDo: remove when not needed
instance NFData MetaVarModifier where
    rnf = genericRnf

metaVarModifierChar :: MetaVarModifier -> Maybe Char
metaVarModifierChar NoMod = Nothing
metaVarModifierChar ModFile = Just 'F'
metaVarModifierChar ModDir = Just 'D'

metaVarString :: MetaVar -> MetaVarModifier -> String
metaVarString metaVar modifier =
    '$' :
    case metaVarModifierChar modifier of
    Nothing -> [varChar]
    Just modChar -> ['(',varChar,modChar,')']
    where
        varChar = metaVarChar metaVar

data Expr3 -- ToDo: rename
    = Expr3'Str ByteString
    | Expr3'Spaces
    | Expr3'VarSpecial MetaVar MetaVarModifier
    deriving (Eq, Ord, Show, Generic)
instance NFData Expr3
instance Binary Expr3 where -- ToDo: remove when not needed

