-- | This is the same as `Control.Comonad.Cofree` from the `purescript-free` package.
-- | However, we need to override the applicative and monad instance
-- | The _cofree comonad_ for a `Functor`.
-- | This version also adds `lazyCofree`, `lazyHead`, `lazyTail`, and `mfix`
-- | And removes the dependency on 'free'
module Control.Cofree
  ( Cofree
  , (:<)
  , buildCofree
  , lazyCofree
  , deferCofree
  , head
  , lazyHead
  , hoistCofree
  , mkCofree
  , tail
  , lazyTail
  , mfix
  , unfoldCofree
  ) where

import Concur.Core.Types (Widget)
import Control.Alternative (class Alternative, (<|>), empty)
import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (<*>))
import Control.Bind (class Bind)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Lazy as Z
import Control.Monad (class Monad, ap)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.BooleanAlgebra ((&&))
import Data.Eq (class Eq, class Eq1, eq, eq1, (==))
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Function (flip, identity, ($), (<<<))
import Data.Functor (class Functor, map, (<$>))
import Data.Lazy (Lazy, defer, force)
import Data.Monoid (class Monoid)
import Data.NaturalTransformation (type (~>))
import Data.Ord (class Ord, class Ord1, compare, compare1)
import Data.Ordering (Ordering(..))
import Data.Semigroup ((<>))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)

-- | The `Cofree` `Comonad` for a functor.
-- |
-- | A value of type `Cofree f a` consists of an `f`-branching
-- | tree, annotated with labels of type `a`.
-- |
-- | The `Comonad` instance supports _redecoration_, recomputing
-- | labels from the local context.
newtype Cofree f a = Cofree (Lazy (Tuple a (f (Cofree f a))))

-- | Lazily creates a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
lazyCofree
  :: forall f a
   . Lazy (Tuple a (f (Cofree f a)))
  -> Cofree f a
lazyCofree = Cofree

-- | Lazily creates a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
deferCofree
  :: forall f a
   . (Unit -> Tuple a (f (Cofree f a)))
  -> Cofree f a
deferCofree = Cofree <<< defer

-- | Create a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
mkCofree
  :: forall f a
   . a
  -> f (Cofree f a)
  -> Cofree f a
mkCofree a t = Cofree
  ( defer \_ ->
      Tuple a t
  )

infixr 5 mkCofree as :<

-- | Returns the label for a tree.
head
  :: forall f a
   . Cofree f a
  -> a
head (Cofree c) = fst (force c)

-- | Returns the "subtrees" of a tree.
tail
  :: forall f a
   . Cofree f a
  -> f (Cofree f a)
tail (Cofree c) = snd (force c)

-- | Like `head`, but returns a lazy value
lazyHead
  :: forall f a
   . Cofree f a
  -> (Lazy a)
lazyHead (Cofree c) = map fst c

-- | Like `tail`, but returns a lazy value
lazyTail
  :: forall f a
   . Cofree f a
  -> (Lazy (f (Cofree f a)))
lazyTail (Cofree c) = map snd c

hoistCofree :: forall f g. Functor f => (f ~> g) -> Cofree f ~> Cofree g
hoistCofree nat (Cofree c) = Cofree (map (nat <<< map (hoistCofree nat)) <$> c)

-- | This signature is deprecated and will be replaced by `buildCofree` in a
-- | future release.
unfoldCofree
  :: forall f s a
   . Functor f
  => (s -> a)
  -> (s -> f s)
  -> s
  -> Cofree f a
unfoldCofree e n = buildCofree
  ( \s ->
      Tuple (e s) (n s)
  )

-- | Recursively unfolds a `Cofree` structure given a seed.
buildCofree
  :: forall f s a
   . Functor f
  => (s -> Tuple a (f s))
  -> s
  -> Cofree f a
buildCofree k s = Cofree
  ( defer \_ ->
      map (buildCofree k) <$> k s
  )

instance eqCofree :: (Eq1 f, Eq a) => Eq (Cofree f a) where
  eq x y = head x == head y && tail x `eq1` tail y

instance eq1Cofree :: (Eq1 f) => Eq1 (Cofree f) where
  eq1 = eq

instance ordCofree :: (Ord1 f, Ord a) => Ord (Cofree f a) where
  compare x y = case compare (head x) (head y) of
    EQ -> compare1 (tail x) (tail y)
    r -> r

instance ord1Cofree :: (Ord1 f) => Ord1 (Cofree f) where
  compare1 = compare

instance functorCofree :: (Functor f) => Functor (Cofree f) where
  map f = loop
    where
    loop (Cofree fa) = Cofree
      ( ( \(Tuple a b) ->
            Tuple (f a) (loop <$> b)
        ) <$> fa
      )

instance foldableCofree :: (Foldable f) => Foldable (Cofree f) where
  foldr f = flip go
    where
    go fa b = f (head fa) (foldr go b (tail fa))
  foldl f = go
    where
    go b fa = foldl go (f b (head fa)) (tail fa)
  foldMap f = go
    where
    go fa = f (head fa) <> (foldMap go (tail fa))

instance traversableCofree :: (Traversable f) => Traversable (Cofree f) where
  sequence = traverse identity
  traverse f = loop
    where
    loop ta = mkCofree <$> f (head ta) <*> (traverse loop (tail ta))

instance extendCofree :: (Functor f) => Extend (Cofree f) where
  extend f = loop
    where
    loop (Cofree fa) = Cofree
      ( ( \(Tuple _ b) ->
            Tuple (f (Cofree fa)) (loop <$> b)
        ) <$> fa
      )

instance comonadCofree :: (Functor f) => Comonad (Cofree f) where
  extract = head

instance applyCofree :: (Alternative f) => Apply (Cofree f) where
  apply = ap

instance applicativeCofree :: (Alternative f) => Applicative (Cofree f) where
  pure a = mkCofree a empty

instance bindCofree :: (Alternative f) => Bind (Cofree f) where
  -- bind :: forall a b. Signal v a -> (a -> Signal v b) -> Signal v b
  bind sa' f = go sa'
    where
    go sa = go' sa (f (head sa))
    go' sa sb =
      let
        mrestart = go <$> tail sa
        msplit = go' sa <$> tail sb
      in
        mkCofree (head sb) (mrestart <|> msplit)

-- instance bindCofree :: Alternative f => Bind (Cofree f) where
--   bind fa f = loop fa
--     where
--     loop fa' =
--       let fh = f (head fa')
--       in mkCofree (head fh) ((tail fh) <|> (loop <$> tail fa'))
instance monadCofree :: (Alternative f) => Monad (Cofree f)

instance isLazyCofree :: Z.Lazy (Cofree f a) where
  defer k = Cofree
    ( defer \_ ->
        let
          (Cofree t) = k unit
        in
          force t
    )

instance shiftMapCofree :: Monoid v => ShiftMap (Widget v) (Cofree (Widget v)) where
  shiftMap f (Cofree l) = deferCofree \_ ->
    let
      Tuple a rest = force l
    in
      Tuple a (f pure (map (shiftMap f) rest))

mfix :: forall f a. (Lazy a -> Cofree f a) -> Cofree f a
mfix f = Z.fix \res -> f $ lazyHead res
