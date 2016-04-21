{-# LANGUAGE ViewPatterns #-} 
module Lib where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Function

prop_identity ::  (Functor f, Show (f a), Eq (f a)) => f a -> Property
prop_identity x = (fmap id x) === x


prop_composition :: (Eq (f c), Functor f, Show (f c)) => f a -> Fun a b -> Fun b c -> Property
prop_composition x (Fun _ f) (Fun _ g) = ((g . f) <$> x) === (g <$> (f <$> x) )

newtype Identity a = Identity a
  deriving (Show, Eq)
instance (Arbitrary a) =>  Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a
  deriving (Show, Eq)
instance (Arbitrary a) =>  Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

data Two a b = Two a b
  deriving (Show, Eq)
instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
  fmap f (Two a b) = Two (a) (f b)

data Three a b c = Three a b c
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
instance Functor (Three a b) where
  fmap f (Three a b c) = (Three a b (f c))

data Three' a b = Three' a b b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
instance Functor (Three' a) where
  fmap f (Three' a b b') = (Three' a (f b) (f b'))

data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = (Four' a a' a'' $ f b)

main = do
  quickCheck (prop_identity :: (Identity Int) -> Property)
  quickCheck (prop_composition :: (Identity Int) -> (Fun Int String) -> (Fun String (Int, Double)) -> Property)

  quickCheck (prop_identity :: (Identity Int) -> Property)
  quickCheck (prop_composition :: (Pair Int) -> (Fun Int String) -> (Fun String (Int, Double)) -> Property)

  quickCheck (prop_identity :: (Two Int String) -> Property)
  quickCheck (prop_composition :: (Two Int String) -> (Fun String String) -> (Fun String (Int, Double)) -> Property)

  quickCheck (prop_identity :: (Three Int Double String) -> Property)
  quickCheck (prop_composition :: (Three Int Double String) -> (Fun String String) -> (Fun String (Int, Double)) -> Property)

  quickCheck (prop_identity :: (Three' Int String) -> Property)
  quickCheck (prop_composition :: (Three' Int String) -> (Fun String String) -> (Fun String (Int, Double)) -> Property)

  quickCheck (prop_identity :: (Four' Int String) -> Property) 
  quickCheck (prop_composition :: (Four' Int String) -> (Fun String String) -> (Fun String (Int, Double)) -> Property)

  quickCheck (prop_identity :: (Either' Int String) -> Property) 
  quickCheck (prop_composition :: (Either' Int String) -> (Fun String String) -> (Fun String (Int, Double)) -> Property)
  print $ mean [1..10]

--take for example
mean xs = uncurry (/) $ foldr f (0, 0) xs
  where f x (len, sum) = (len + 1, sum + x) 

-- the kind of a Type implementing Functor must be * -> *. So you have to partially aply in cases like (a, b) or Two or Either'.
-- something like an Enum cannot be a functor.
--
--
data Either' a b = Left' a | Right' b
  deriving (Show, Eq)
instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Either' a b) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance Functor (Either' a) where
  fmap f (Left' x)   = Left' x
  fmap f (Right' y)  = Right' $ f y
