{-# LANGUAGE 
    PolyKinds 
  , FunctionalDependencies , FlexibleInstances , FlexibleContexts
  , OverlappingInstances
  , UndecidableInstances
  , ScopedTypeVariables
  , TypeFamilies
  , DataKinds -- for lifted Bool
  , UndecidableInstances -- somewhat sure all possible cases are decidable...
 #-}
module Data.Proxy.Kindness (
{- |
/NOTE/: All classes here should be considered closed.
-}
    -- * Manipulating Proxy types
      Applied(..), Unapplied(..)
    -- ** Composing and decomposing Proxy types
    , unap, ap
    -- * Using proxy types with real values
    , asApplied, unappliedOf
    -- * Predicate classes
    , IsOfBaseType, AreEqUnapplied
    ) where

import Data.Proxy

-- | A relation between a (maybe-partially-applied) type and that type fully
-- applied.
class Applied t (tab :: *) | t -> tab where
    -- | Fully apply a type @t@ with polymorphic arguments, yielding @tab@.
    applied :: Proxy t -> Proxy tab

instance Applied (t a) tab=> Applied t tab where
    applied _ = applied (Proxy :: Proxy (t a))

instance t ~ tab=> Applied t tab where -- always matches when `t` is kind `*`
    applied _ = Proxy :: Proxy tab


-- | Create a proxy value for the completely 'unapplied' type of @tab@
unappliedOf :: (Unapplied (Proxy tab) t)=> tab -> Proxy t
unappliedOf = unapplied . proxyOf
-- unappliedOf tab = unapplied (Proxy :: Proxy tab) -- TODO: and why doesn't this work?

-- | Force the first argument to the fully 'applied' proxy type of the second, e.g.
--
-- >>> let x = (Left 1) `asApplied` (Proxy :: Proxy (Either Double))
-- >>> :t x 
-- x :: Either Double a
asApplied :: (Applied ta tab)=> tab -> Proxy ta -> tab
asApplied tab = asProxyTypeOf tab . applied

proxyOf :: a -> Proxy a -- unnecessary when eventually unapplied :: Proxy tab -> Proxy t
proxyOf = return

{-
as :: a -> Proxy a -> a
as = asProxyTypeOf
-}

-- TODO: test in GHC 7.8. Replace `t` with a closed type synonym instance if possible.
-- | A relation between a (maybe-partially-applied) type and that type stripped
-- of all its arguments.
--
-- When the bare type is ambiguous (e.g. @unapplied (p :: Proxy (m a))@), an
-- overlapping instances type error will be raised.
class Unapplied ptab t | ptab -> t where
    -- | Given a @Proxy tab@, strip away all of its arguments, leaving the type
    -- @t@.
    unapplied :: ptab -> Proxy t

instance (Unapplied (Proxy ta) t)=> Unapplied (Proxy (ta b)) t where
    unapplied _ = unapplied (Proxy :: Proxy ta)

-- GHC 7.6.3 sees ambiguous overlap seemingly related to ~ here (which doesn't
-- make much sense), so I can't seem to give unapplied the signature I'd like:
-- unapplied :: Proxy tab -> Proxy t
instance (pt ~ Proxy t)=> Unapplied pt t where
    unapplied _ = Proxy :: Proxy t


unap :: Proxy (t a) -> (Proxy t, Proxy a)
unap _ = (Proxy, Proxy)

ap :: Proxy t -> Proxy a -> Proxy (t a)
ap _ _ = Proxy

-- TODO try closed type fam
-- | A predicate class that returns @True@ when @t@ is a partially-applied
-- \"prefix\" type of @tab@, i.e. @tab@ can be 'unap'-ed to @t@.
class IsOfBaseType t tab (b :: Bool) | t tab -> b
    -- where test :: Proxy t -> Proxy tab -> IO ()
instance IsOfBaseType t t True
    -- where test _ _ = putStrLn "Yes1"
instance IsOfBaseType (t a) (t a) True
    -- where test _ _ = putStrLn "Yes2"
instance (IsOfBaseType t ta bool)=> IsOfBaseType t (ta b) bool
    -- where test _ _ = putStrLn "Going..." >> test (Proxy :: Proxy t) (Proxy :: Proxy ta)
instance false ~ False => IsOfBaseType t x false
    -- where test _ _ = putStrLn "No"

-- | A predicate class that returns @True@ when the unapplied "base" type of
-- @ta@ and @tb@ are identical. 
class AreEqUnapplied ta tb (b :: Bool) | ta tb -> b
    -- where test1 :: Proxy ta -> Proxy tb -> IO ()
instance (Unapplied (Proxy ta) t, IsOfBaseType t tb b)=> AreEqUnapplied ta tb b
    -- where test1 = test . unapplied
