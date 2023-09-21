{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use foldl" #-}
module Lib
    ( GenPropLogic(..)
    , BinaryOp(..)
    , interpret
    , propLogicSize
    , foldLeft
    , foldLeft'
    , foldRight
    , UserRecord(..)
    , validateUser
    ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class (lift)

type PropLogic = GenPropLogic String
data GenPropLogic a
    = PLAtomic a
    | PLBinary BinaryOp (GenPropLogic a) (GenPropLogic a)
    deriving (Show, Eq)

data BinaryOp = BinOpAnd | BinOpOr | BinOpIfThen | BinOpIfAndOnlyIf deriving (Show, Eq)

toLogicalOp :: BinaryOp -> (Bool -> Bool -> Bool)
toLogicalOp BinOpAnd = (&&)
toLogicalOp BinOpOr = (||)
toLogicalOp BinOpIfThen = if'
toLogicalOp BinOpIfAndOnlyIf = \a b -> (a `if'` b) && (b `if'` a)

if' :: Bool -> Bool -> Bool
if' a b = if a then b else True

propLogicSize :: PropLogic -> Int
propLogicSize = foldr (\_ size -> size + 1) 0

instance Foldable GenPropLogic where
    foldr :: (a -> b -> b) -> b -> GenPropLogic a -> b
    foldr f z (PLAtomic a) = f a z
    foldr f z (PLBinary _ l r) = foldr f (foldr f z r) l

interpret :: (Show a, Ord a) => GenPropLogic a -> Map a Bool -> Either String Bool
interpret gpl = runReader (runExceptT $ interpret' gpl)

interpret' :: (Show a, Ord a) => GenPropLogic a -> ExceptT String (Reader (Map a Bool)) Bool
interpret' (PLAtomic a) = do
    mapping <- lift ask
    let mbValue = lookup a mapping
    case mbValue of
      Just value -> return value
      Nothing -> throwE $ show a <> " is not in the mapping!"
interpret' (PLBinary op a b) = do
    aResult <- interpret' a
    bResult <- interpret' b
    return $ toLogicalOp op aResult bResult

-- ///////////////////////

foldLeft :: a -> (a -> b -> a) -> [b] -> a
foldLeft z _f [] = z
foldLeft z f (b : bs) = foldLeft (f z b) f bs

foldLeft' :: a -> (a -> b -> a) -> [b] -> a
foldLeft' z _f [] = z
foldLeft' z f (b : bs) = 
    let fResult = f z b in
    fResult `seq` foldLeft fResult f bs

foldRight :: a -> (b -> a -> a) -> [b] -> a
foldRight z _f [] = z
foldRight z f (b : bs) = f b (foldRight z f bs)

-- ///////////////////////

data Validation a
    = Ok a
    | Failed String

instance Functor Validation where
    fmap :: (a -> b) -> Validation a -> Validation b
    fmap f (Ok a) = Ok $ f a
    fmap _ (Failed err) = Failed err

instance Applicative Validation where
  pure :: a -> Validation a
  pure = Ok
  (<*>) :: Validation (a -> b) -> Validation a -> Validation b
  (<*>) (Ok f) (Ok a) = Ok $ f a
  (<*>) _ (Failed err) = Failed err
  (<*>) (Failed err) _ = Failed err

data UserRecord
    = UserRecord
    { userName :: String
    , userAddress :: String
    , userAge :: Int
    }

validateUser :: String -> String -> Int -> Either String UserRecord
validateUser name address age =
    case userValidation name address age of
        Ok user -> Right user
        Failed err -> Left err

userValidation :: String -> String -> Int -> Validation UserRecord
userValidation name address age =
    UserRecord <$> nameValidation name <*> addressValidation address <*> ageValidation age

nameValidation :: String -> Validation String
nameValidation = Ok

addressValidation :: String -> Validation String
addressValidation = Ok

ageValidation :: Int -> Validation Int
ageValidation age =
    if age >= 18
    then Ok age
    else Failed "Must be above 18"
