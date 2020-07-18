module Identy.ObjectMap
  ( ObjectMap
  , keys
  , values
  , update
  , union
  , size
  , singleton
  , member
  , lookup
  , isEmpty
  , insert
  , empty
  , delete
  , alter
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable)
import Foreign.Object as Object
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype ObjectMap k v = ObjectMap (Object.Object v)

derive newtype instance eqObjectMap :: (Eq v) => Eq (ObjectMap k v)
derive newtype instance showObjectMap :: (Show v) => Show (ObjectMap k v)
derive newtype instance semigroupObjectMap :: (Semigroup v) => Semigroup (ObjectMap k v)
derive newtype instance monoidObjectMap :: (Semigroup v) => Monoid (ObjectMap k v)
derive newtype instance functorObjectMap :: Functor (ObjectMap k)
derive newtype instance foldableObjectMap :: Foldable (ObjectMap k)
derive newtype instance traversableObjectMap :: Traversable (ObjectMap k)

instance readForeignObjectMap :: (Newtype k String, ReadForeign v) => ReadForeign (ObjectMap k v) where
  readImpl obj = ObjectMap <$> readImpl obj

instance writeForeignObjectMap :: (Newtype k String, WriteForeign v) => WriteForeign (ObjectMap k v) where
  writeImpl (ObjectMap obj) = writeImpl obj

keys :: forall k v. Newtype k String => ObjectMap k v -> Array k
keys (ObjectMap obj) = wrap <$> (Object.keys obj)

values :: forall k v. ObjectMap k v -> Array v
values (ObjectMap obj) = Object.values obj

update :: forall k v. Newtype k String => (v -> Maybe v) -> k -> ObjectMap k v -> ObjectMap k v
update f k (ObjectMap obj) = ObjectMap $ Object.update f (unwrap k) obj

union :: forall k v. ObjectMap k v -> ObjectMap k v -> ObjectMap k v
union (ObjectMap lhs) (ObjectMap rhs) = ObjectMap $ Object.union lhs rhs

size :: forall k v. ObjectMap k v -> Int
size (ObjectMap obj) = Object.size obj

singleton :: forall k v. Newtype k String => k -> v -> ObjectMap k v
singleton k v = ObjectMap $ Object.singleton (unwrap k) v

member :: forall k v. Newtype k String => k -> ObjectMap k v -> Boolean
member k (ObjectMap obj) = Object.member (unwrap k) obj

lookup :: forall k v. Newtype k String => k -> ObjectMap k v -> Maybe v
lookup k (ObjectMap obj) = Object.lookup (unwrap k) obj

isEmpty :: forall k v. ObjectMap k v -> Boolean
isEmpty (ObjectMap obj) = Object.isEmpty obj

insert :: forall k v. Newtype k String => k -> v -> ObjectMap k v -> ObjectMap k v
insert k v (ObjectMap obj) = ObjectMap $ Object.insert (unwrap k) v obj

empty :: forall k v. ObjectMap k v
empty = ObjectMap Object.empty

delete :: forall k v. Newtype k String => k -> ObjectMap k v -> ObjectMap k v
delete k (ObjectMap obj) = ObjectMap $ Object.delete (unwrap k) obj

alter :: forall k v. Newtype k String => (Maybe v -> Maybe v) -> k -> ObjectMap k v -> ObjectMap k v
alter f k (ObjectMap obj) = ObjectMap $ Object.alter f (unwrap k) obj
