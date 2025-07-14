{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}

module Core where

import Prelude
-- import Prelude.Compat as P
import Data.List (sortOn,groupBy,minimumBy)
import Data.Function (on)
import Data.Semigroup
import Data.Sequence (singleton, Seq, viewl, viewr, ViewL(..), ViewR(..), (|>), (<|), (><), zipWith)
import Data.String
import Data.Foldable (toList)
import Data.Traversable
import Control.Applicative (liftA2)
import qualified Control.Applicative as Seq
-- | Annotated string, which consists of segments with separate (or no) annotations.
--
-- We keep annotated segments in a container (list).
-- The annotation is @Maybe a@, because the no-annotation case is common.
--
-- /Note:/ with @Last x@ annotation, the 'annotate' will overwrite all annotations.
--
-- /Note:/ if the list is changed into `Seq` or similar structure
-- allowing fast viewr and viewl, then we can impose an additional
-- invariant that there aren't two consequtive non-annotated segments;
-- yet there is no performance reason to do so.
--
data AS a = AS !Int [(a, String)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- | Tests the invariants of 'AS'
_validAs :: AS a -> Bool
_validAs (AS i s) = lengthInvariant && noNewlineInvariant
  where
    lengthInvariant = i == sum (map (length . snd) s)
    noNewlineInvariant = all (notElem '\n' . snd) s

asLength :: AS a -> Int
asLength (AS l _) = l

-- | Make a non-annotated 'AS'.
mkAS :: Monoid a => String -> AS a
mkAS s = AS (length s) [(mempty, s)]

instance Semigroup (AS a) where
  AS i xs <> AS j ys = AS (i + j) (xs <> ys)

newtype L a = L (Seq (AS a)) -- non-empty sequence
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Monoid a => Semigroup (L a) where
  L (viewr -> xs :> x) <> L (viewl -> y :< ys) =
    L (xs <> singleton (x <> y) <> indent ys) where

    n      = asLength x
    pad    = mkAS (replicate n ' ')
    indent = if n == 0 then id else fmap (pad <>)

  L _ <> L _ = error "<> @L: invariant violated, Seq is empty"

instance Monoid a => Monoid (L a) where
   mempty = L (singleton (mkAS ""))

instance Layout L where
   text :: Monoid a => String -> L a
   text = L . singleton . mkAS

   flush :: Monoid a => L a -> L a
   flush (L xs) = L (xs |> mkAS "")

   annotate :: Monoid a => a -> L a -> L a
   annotate a (L s') = L (fmap annotateAS s')
      where annotateAS (AS i s) = AS i (fmap annotatePart s)
            annotatePart (b, s) = (b `mappend` a, s)

renderWithL :: (Monoid a, Monoid r) => Options a r -> L a -> r
renderWithL opts (L xs) = intercalate (toList xs)
  where
    f = optsAnnotate opts
    f' (AS _ s) = foldMap (uncurry f) s
    sep = f mempty "\n"

    intercalate []     = mempty
    intercalate (y:ys) = f' y `mappend` foldMap (mappend sep . f') ys

data Options a r = Options
    { optsPageWidth :: !Int              -- ^ maximum page width
    , optsAnnotate  :: a -> String -> r  -- ^ how to annotate the string. /Note:/ the annotation should preserve the visible length of the string.
    }

class Layout d where
  text :: Monoid a => String -> d a
  flush :: Monoid a => d a -> d a
  -- | `<>` new annotation to the 'Doc'.
  --
  -- Example: 'Any True' annotation will transform the rendered 'Doc' into uppercase:
  --
  -- >>> let r = putStrLn . renderWith defaultOptions { optsAnnotate = \a x -> if a == Any True then map toUpper x else x }
  -- >>> r $ text "hello" <$$> annotate (Any True) (text "world")
  -- hello
  -- WORLD
  --
  annotate :: forall a. Monoid a => a -> d a -> d a

-- type parameter is phantom.
data M a = M {height    :: Int,
              lastWidth :: Int,
              maxWidth  :: Int
              }
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance Semigroup (M a) where
  a <> b =
    M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
       height = height a + height b,
       lastWidth = lastWidth a + lastWidth b}

instance Monoid a => Monoid (M a) where
  mempty = text ""
  mappend = (<>)

instance Layout M where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}
  annotate _ M{..} = M{..}
class Poset a where
  (<<<) :: a -> a -> Bool


instance Poset (M a) where
  M c1 l1 s1 <<< M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn m = go
  where
    go [] xs = xs
    go xs [] = xs
    go (x:xs) (y:ys)
      | m x <= m y  = x:go xs (y:ys)
      | otherwise    = y:go (x:xs) ys

mergeAllOn :: Ord b => (a -> b) -> [[a]] -> [a]
mergeAllOn _ [] = []
mergeAllOn m (x:xs) = mergeOn m x (mergeAllOn m xs)

bestsOn :: forall a b. (Poset b, Ord b)
      => (a -> b) -- ^ measure
      -> [[a]] -> [a]
bestsOn m = paretoOn' m [] . mergeAllOn m

-- | @paretoOn m = paretoOn' m []@
paretoOn' :: Poset b => (a -> b) -> [a] -> [a] -> [a]
paretoOn' _ acc [] = reverse acc
paretoOn' m acc (x:xs) = if any ((<<< m x) . m) acc
                            then paretoOn' m acc xs
                            else paretoOn' m (x:acc) xs
                            -- because of the ordering, we have that
                            -- for all y ∈ acc, y <= x, and thus x ≺ y
                            -- is false. No need to refilter acc.

-- list sorted by lexicographic order for the first component
-- function argument is the page width
newtype ODoc a = MkDoc {fromDoc :: Int -> [Pair M L a]}
  deriving (Functor)

instance Monoid a => Semigroup (ODoc a) where
  (<>) :: Monoid a => ODoc a -> ODoc a -> ODoc a
  MkDoc xs <> MkDoc ys = MkDoc $ \w -> bestsOn frst [ discardInvalid w [x <> y | y <- ys w] | x <- xs w]

discardInvalid w = quasifilter (fits w . frst)

quasifilter :: forall {k} {g :: k -> *} {a :: k}. (Pair M g a -> Bool) -> [Pair M g a] -> [Pair M g a]
quasifilter _ [] = []
quasifilter p zs = let fzs = filter p zs
                   in if null fzs -- in case that there are no valid layouts, we take a narrow one.
                      then [minimumBy (compare `on` (maxWidth . frst)) zs]
                      else fzs

instance Monoid a => Monoid (ODoc a) where
  mempty = text ""
  mappend = (<>)

fits :: Int -> M a -> Bool
fits w x = maxWidth x <= w

instance Layout ODoc where
  text s = MkDoc $ \_ -> [text s]
  flush (MkDoc xs) = MkDoc $ \w -> fmap flush (xs w)
  annotate a (MkDoc xs) = MkDoc $ \w -> fmap (annotate a) (xs w)

renderWith :: (Monoid r, Annotation a)
           => Options a r  -- ^ rendering options
           -> ODoc a          -- ^ renderable
           -> r
renderWith opts d = case xs of
    [] -> error "No suitable layout found."
    ((_ :-: x):_) -> renderWithL opts x
  where
    pageWidth = optsPageWidth opts
    xs = discardInvalid pageWidth (fromDoc d pageWidth)

onlySingleLine :: [Pair M L a] -> [Pair M L a]
onlySingleLine = takeWhile (\(M{..} :-: _) -> height == 0)

spaces :: (Monoid a,Layout l) => Int -> l a
spaces n = text $ replicate n ' '


-- | The document @(x \$$> y)@ concatenates document @x@ and @y@ with
-- a linebreak in between. (infixr 5)
($$) :: (Layout d, Monoid a, Semigroup (d a)) => d a -> d a -> d a
a $$ b = flush a <> b

second f (a,b) = (a, f b)

groupingBy :: Monoid a => String -> [(Int,Doc a)] -> Doc a
groupingBy _ [] = mempty
groupingBy separator ms = MkDoc $ \w ->
  let mws = map (second (($ w) . fromDoc)) ms
      (_,lastMw) = last mws
      hcatElems = map (onlySingleLine . snd) (init mws) ++ [lastMw] -- all the elements except the first must fit on a single line
      vcatElems = map (\(indent,x) -> map (spaces indent <>) x) mws
      horizontal = discardInvalid w $ foldr1 (liftA2 (\x y -> x <> text separator <> y)) hcatElems
      vertical = foldr1 (\xs ys -> bestsOn frst [[x $$ y | y <- ys] | x <- xs]) vcatElems
  in bestsOn frst [horizontal,vertical]

data Pair f g a = (:-:) {frst :: f a, scnd :: g a}
  deriving (Functor,Foldable,Traversable, Show)

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Pair f g a) where
  (x :-: y) <> (x' :-: y') = (x <> x') :-: (y <> y')
instance (Monoid (f a), Monoid (g a)) => Monoid (Pair f g a) where
  mempty = mempty :-: mempty

instance (Layout a, Layout b) => Layout (Pair a b) where
  text s = text s :-: text s
  flush (a:-:b) = (flush a:-: flush b)
  annotate x (a:-:b) = (annotate x a:-:annotate x b)

instance Monoid a => IsString (Doc a) where
  fromString = text

type Annotation a = (Monoid a)
type Doc = ODoc

-- tt :: Doc ()
-- tt = groupingBy " " $ map (4,) $ 
--      ((replicate 4 $ groupingBy " " (map (4,) (map text ["fw"]))) ++
--       [groupingBy " " (map (0,) (map text ["fw","arstnwfyut","arstin","arstaruf"]))])

-- $setup
-- >>> import Text.PrettyPrint.Compact
-- >>> import Data.Monoid
-- >>> import Data.Char
  -- MkDoc xs <> MkDoc ys = MkDoc $ \w -> bestsOn frst [ discardInvalid w [x <> y | y <- ys w] | x <- xs w]

fuck :: Monoid a => ODoc a -> ODoc a -> ODoc a
fuck d1 d2 = MkDoc (\w -> do
  let x1 = fromDoc d1 w
      --x2 = fromDoc d2 (w `div` 2)

      elems = bestsOn frst [ discardInvalid w [xs `dumb` ys | ys <- fromDoc d2 (w - maxWidth (frst xs))] | xs <- x1 ]

      dumb (mx :-: L xs) (my :-: L ys) =
        let m = M {height = max (height mx) (height my), maxWidth = maxWidth mx + maxWidth my, lastWidth = lastWidth my} in
        m :-: L (extZip (maxWidth mx) xs ys)

      extZip :: Int -> Seq (AS a) -> Seq (AS a) -> Seq (AS a)
      extZip w xs ys
        | null xs && null ys = Seq.empty
        | null xs = fmap (AS w [(undefined, replicate w ' ')] <>) ys
        | null ys = xs
        | otherwise = 
          let (viewl -> hx :< tx) = xs
              (viewl -> hy :< ty) = ys 
              (AS l _) = hx
              line = hx <> AS (w - l) [(undefined, replicate (w - l) ' ')] <> hy
          in line <| extZip w tx ty
  -- 
  elems
  )

