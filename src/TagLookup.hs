module TagLookup where
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

data TaggedMap k v = TaggedMap {
    nextTag :: M.Map k (TaggedMap k v),
    vals :: [v]
}

emptyMap :: TaggedMap k v
emptyMap = TaggedMap M.empty []

insertTg :: forall k v . Ord k => v -> S.Set k -> TaggedMap k v -> TaggedMap k v
insertTg v rawTags = go ordTags
    where ordTags = S.toAscList rawTags
          go :: [k] -> TaggedMap k v -> TaggedMap k v
          go [] (TaggedMap ntags vs) = TaggedMap ntags (v : vs)
          go (k: ks) (TaggedMap ntags vs) = let 
                ifEmpty = go ks emptyMap
                newTags = M.insertWith f k ifEmpty ntags
                f tm _ = go ks tm
              in TaggedMap newTags vs

lookup :: Ord k => S.Set k -> TaggedMap k v -> [v]
lookup rawTags = go ordTags
    where ordTags = S.toAscList rawTags 
          go [] (TaggedMap _ vs) = vs
          go (x: xs) (TaggedMap mp _) = maybe [] (go xs) (M.lookup x mp)


remove :: (Ord k, Eq v) => v -> S.Set k -> TaggedMap k v -> TaggedMap k v
remove v rawTags = go ordTags
    where ordTags = S.toAscList rawTags 
          go [] (TaggedMap mp vs) = TaggedMap mp (L.delete v vs)
          go (x: xs) extM@(TaggedMap mp _) = maybe extM (go xs) (M.lookup x mp)

