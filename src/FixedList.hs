{-# LANGUAGE LiberalTypeSynonyms        #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}

module FixedList
  ( AsL4( l4 ), AsL5( l5 ), AsL6( l6 ), AsL8( l8 )
  , L4( L4 ), L5( L5 ), L6( L6 ), L8( L8 ) )
where

-- base --------------------------------

import Control.Applicative  ( Applicative( (<*>), pure ) )
import Data.Foldable        ( Foldable( foldl', foldl1, foldr, foldr1
                                      , foldMap ) )
import Data.Function        ( id )
import Data.Functor         ( Functor( fmap ), (<$>) )
import Data.Traversable     ( Traversable( traverse ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-monotraversable ----------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldMap, ofoldl', ofoldl1Ex'
                                           , ofoldr1Ex, ofoldr, otoList )
                             , MonoFunctor( omap )
                             , MonoTraversable( omapM, otraverse ) )

-- lens --------------------------------

import Control.Lens.Iso   ( Iso )
import Control.Lens.Type  ( Simple )

--------------------------------------------------------------------------------

{- | A list of length 4. -}
data L4 α = L4 α α α α

type instance Element (L4 α) = α

instance Functor L4 where
  fmap f (L4 a b c d) = L4 (f a) (f b) (f c) (f d)

instance Applicative L4 where
  pure a = L4 a a a a
  L4 f0 f1 f2 f3 <*> L4 a0 a1 a2 a3 = L4 (f0 a0) (f1 a1) (f2 a2) (f3 a3)

instance Foldable L4 where
  foldr f x (L4 a b c d) = foldr f x [a,b,c,d]

instance Traversable L4 where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse f (L4 a b c d) = L4 <$> f a <*> f b <*> f c <*> f d

instance MonoFunctor (L4 α) where
  omap f ls = fmap f ls

instance MonoFoldable (L4 α) where
  otoList (L4 a b c d) = [a,b,c,d]
  ofoldl'    f x       = foldl' f x ∘ otoList
  ofoldr     f x       = foldr f x ∘ otoList
  ofoldMap   f         = foldMap f ∘ otoList
  ofoldr1Ex  f         = foldr1 f ∘ otoList
  ofoldl1Ex' f         = foldl1 f ∘ otoList

instance MonoTraversable (L4 α) where
  omapM = otraverse

class AsL4 α where
  l4 ∷ Simple Iso α (L4 (Element α))

instance AsL4 (L4 α) where
  l4 = id

------------------------------------------------------------

{- | A list of length 5. -}
data L5 α = L5 α α α α α

type instance Element (L5 α) = α

class AsL5 α where
  l5 ∷ Simple Iso α (L5 (Element α))

instance AsL5 (L5 α) where
  l5 = id

instance Functor L5 where
  fmap f (L5 a b c d e) = L5 (f a) (f b) (f c) (f d) (f e)

instance MonoFunctor (L5 α) where
  omap f ls = fmap f ls

instance Foldable L5 where
  foldr f x (L5 a b c d e) = foldr f x [a,b,c,d,e]

instance MonoFoldable (L5 α) where
  otoList (L5 a b c d e) = [a,b,c,d,e]
  ofoldl'    f x         = foldl' f x ∘ otoList
  ofoldr     f x         = foldr f x ∘ otoList
  ofoldMap   f           = foldMap f ∘ otoList
  ofoldr1Ex  f           = foldr1 f ∘ otoList
  ofoldl1Ex' f           = foldl1 f ∘ otoList

------------------------------------------------------------

{- | A list of length 6. -}
data L6 α = L6 α α α α α α

type instance Element (L6 α) = α

class AsL6 α where
  l6 ∷ Simple Iso α (L6 (Element α))

instance AsL6 (L6 α) where
  l6 = id

instance Functor L6 where
  fmap g (L6 a b c d e f) = L6 (g a) (g b) (g c) (g d) (g e) (g f)

instance Applicative L6 where
  pure a = L6 a a a a a a
  L6 f0 f1 f2 f3 f4 f5 <*> L6 a0 a1 a2 a3 a4 a5 =
    L6 (f0 a0) (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)

instance Foldable L6 where
  foldr g x (L6 a b c d e f) = foldr g x [a,b,c,d,e,f]

instance Traversable L6 where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse g (L6 a b c d e f) =
    L6 <$> g a <*> g b <*> g c <*> g d <*> g e <*> g f

instance MonoFoldable (L6 α) where
  otoList (L6 a b c d e f) = [a,b,c,d,e,f]
  ofoldl'    f x           = foldl' f x ∘ otoList
  ofoldr     f x           = foldr f x ∘ otoList
  ofoldMap   f             = foldMap f ∘ otoList
  ofoldr1Ex  f             = foldr1 f ∘ otoList
  ofoldl1Ex' f             = foldl1 f ∘ otoList

------------------------------------------------------------

{- | A list of length 8. -}
data L8 α = L8 α α α α α α α α

type instance Element (L8 α) = α

class AsL8 α where
  l8 ∷ Simple Iso α (L8 (Element α))

instance AsL8 (L8 α) where
  l8 = id

instance Traversable L8 where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse p (L8 a b c d e f g h) =
    L8 <$> p a <*> p b <*> p c <*> p d <*> p e <*> p f <*> p g <*> p h

instance Functor L8 where
  fmap p (L8 a b c d e f g h) =
    L8 (p a) (p b) (p c) (p d) (p e) (p f) (p g) (p h)

instance Foldable L8 where
  foldr p x (L8 a b c d e f g h) = foldr p x [a,b,c,d,e,f,g,h]

-- that's all, folks! ----------------------------------------------------------
