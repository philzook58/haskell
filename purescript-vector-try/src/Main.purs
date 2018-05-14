module Main where

import Control.Comonad.Cofree
import DOM
import DOM.HTML
import DOM.HTML.Document
import DOM.HTML.Types
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.Document
import DOM.Node.Node
import DOM.Node.NonElementParentNode
import DOM.Node.Types
import Data.Array hiding (index)
import Data.BinVec
import Data.Enum
import Data.Function.Uncurried
import Data.Functor.Representable
import Data.Int
import Data.Maybe
import Math
import Partial.Unsafe
import Prelude
import Unsafe.Coerce

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
--import Unsafe.Partial
--import Data.Tensor

foreign import plotlyImpl :: forall a e. Fn3 Element (Array a) (Array a) (Eff (dom :: DOM | e) Unit)
plotly :: forall a e. Element -> Array a -> Array a -> Eff (dom :: DOM | e) Unit
plotly = runFn3 plotlyImpl
{-
plotT :: forall a e. String -> Tensor a -> Tensor a -> Eff (dom :: DOM | e) Unit
plotT divid x y = plotly divid (asArray x) (asArray y)
-}
{-
foreign import heatmapImpl :: forall a e. Fn2 String (Array (Array a)) (Eff (dom :: DOM | e) Unit)
heatmap :: forall a e. String -> Array (Array a) -> Eff (dom :: DOM | e) Unit
heatmap = runFn2 heatmapImpl
heatmapT divid z = heatmap divid (unconcat n (asArray z)) where n = fromMaybe 0 $ (shape z) !! 0

foreign import surfaceImpl :: forall a e. Fn2 String (Array (Array a)) (Eff (dom :: DOM | e) Unit)
surface :: forall a e. String -> Array (Array a) -> Eff (dom :: DOM | e) Unit
surface = runFn2 surfaceImpl
surfaceT divid z = surface divid (unconcat n (asArray z)) where n = fromMaybe 0 $ (shape z) !! 0
-}

newdiv :: forall a e. Eff (console :: CONSOLE, dom::DOM | e) Element
newdiv = do
  w <- window
  doc <- document w
  let doc' = htmlDocumentToDocument doc
  bod <- body doc
  p1 <- createElement  "div" doc'
  element <- getElementById (ElementId "main") $ htmlDocumentToNonElementParentNode doc
  _ <- appendChild (elementToNode p1) (elementToNode (unsafePartial (fromJust element)))
  pure p1

-- This is a useful function.
linspace :: forall f a. BoundedEnum a => Representable f a => Number -> Number -> f Number 
linspace a b = map (\i -> delta * (toNumber i)) fillRange where
												Cardinality n = cardinality :: Cardinality a
												delta = (b - a)/ (toNumber n)
psi :: Int -> Number -> Number
psi n x = sin ((toNumber n) * pi * x)

toArray' ::forall f a b. BoundedEnum a => Representable f a => f b -> Array b
toArray' x = map (index x) basis

p = fillFromIndex (\i -> psi 1 (toNumber i)) :: V8 Number
v = linspace 0.0 1.0 :: V8 Number
v2 :: _
v2 = (map (psi 1) v)

-- Wait. ToArray doesn't work on General V?


main :: forall e. Eff (console :: CONSOLE, dom::DOM | e) Unit
main = do
  log "Hello sailor!"
  p1 <- newdiv
  plotly p1 (range 0 10) (range 0 10)
  --p2 <- newdiv
  --plotly p2 (range 0 10) (replicate 10 0)
  --p3 <- newdiv
  --plotly p3 (toArray' v) (toArray' v)
  log $ show (toArray' v)


  --fillRange :: V8 Int
  

