-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Primitives
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Primitives
   ( pixel
   , hLine
   , vLine
   , rectangle
   , circle
   , box
   , line
   , aaLine
   , arc
   , aaCircle
   , filledCircle
   , ellipse
   , aaEllipse
   , filledEllipse
   , pie
   , filledPie
   , trigon
   , filledTrigon
   , aaTrigon
   , polygon
   , texturedPolygon
   , filledPolygon
   , aaPolygon
   , bezier
   ) where

import Foreign as Foreign hiding (new)
import Foreign.C

import Graphics.UI.SDL.General
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Utilities (intToBool, toBitmask, fromCInt, toCInt)

foreign import ccall unsafe "pixelColor"
    gfxPixelColor :: Ptr RendererStruct -> Int16 -> Int16 -> Word32 -> IO CInt

pixel :: Renderer -> Int16 -> Int16 -> Pixel -> IO Bool
pixel renderer x y (Pixel w) = withForeignPtr renderer $ \ptr ->
                              intToBool (-1) (fmap fromCInt $ gfxPixelColor ptr x y w)

foreign import ccall unsafe "hlineColor" gfxHLineColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

hLine :: Renderer -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
hLine renderer x1 x2 y (Pixel w) = withForeignPtr renderer $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxHLineColor ptr x1 x2 y w)

foreign import ccall unsafe "vlineColor" gfxVLineColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

vLine :: Renderer -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
vLine renderer x y1 y2 (Pixel w) = withForeignPtr renderer $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxVLineColor ptr x y1 y2 w)

foreign import ccall unsafe "rectangleColor" gfxRectangleColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

rectangle :: Renderer -> Rect -> Pixel -> IO Bool
rectangle renderer (Rect x y w h) (Pixel c) = withForeignPtr renderer $ \ptr ->
                                             intToBool (-1) (fmap fromCInt $ gfxRectangleColor ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) c)

foreign import ccall unsafe "boxColor" gfxBoxColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

box :: Renderer -> Rect -> Pixel -> IO Bool
box renderer (Rect x y w h) (Pixel c) = withForeignPtr renderer $ \ptr ->
                                       intToBool (-1) (fmap fromCInt $ gfxBoxColor ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) c)

foreign import ccall unsafe "lineColor" gfxLineColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

line :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
line renderer x y x' y' (Pixel w) = withForeignPtr renderer $ \ptr ->
                                   intToBool (-1) (fmap fromCInt $ gfxLineColor ptr x y x' y' w)

foreign import ccall unsafe "aalineColor" gfxAALineColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaLine :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaLine renderer x y x' y' (Pixel w) = withForeignPtr renderer $ \ptr ->
                                     intToBool (-1) (fmap fromCInt $ gfxAALineColor ptr x y x' y' w)

foreign import ccall unsafe "circleColor" gfxCircleColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

circle :: Renderer -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
circle renderer x y r (Pixel w) = withForeignPtr renderer $ \ptr ->
                                 intToBool (-1) (fmap fromCInt $ gfxCircleColor ptr x y r w)

foreign import ccall unsafe "arcColor" gfxArcColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

arc :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
arc renderer x y r s e (Pixel w) = withForeignPtr renderer $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxArcColor ptr x y r s e w)

foreign import ccall unsafe "aacircleColor" gfxAACircleColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaCircle :: Renderer -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaCircle renderer x y r (Pixel w) = withForeignPtr renderer $ \ptr ->
                                   intToBool (-1) (fmap fromCInt $ gfxAACircleColor ptr x y r w)

foreign import ccall unsafe "filledCircleColor" gfxFilledCircleColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledCircle :: Renderer -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledCircle renderer x y r (Pixel w) = withForeignPtr renderer $ \ptr ->
                                       intToBool (-1) (fmap fromCInt $ gfxFilledCircleColor ptr x y r w)

foreign import ccall unsafe "ellipseColor" gfxEllipseColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

ellipse :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
ellipse renderer x y rx ry (Pixel w) = withForeignPtr renderer $ \ptr ->
                                      intToBool (-1) (fmap fromCInt $ gfxEllipseColor ptr x y rx ry w)

foreign import ccall unsafe "aaellipseColor" gfxAAEllipseColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaEllipse :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaEllipse renderer x y rx ry (Pixel w) = withForeignPtr renderer $ \ptr ->
                                        intToBool (-1) (fmap fromCInt $ gfxAAEllipseColor ptr x y rx ry w)

foreign import ccall unsafe "filledEllipseColor" gfxFilledEllipseColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledEllipse :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledEllipse renderer x y rx ry (Pixel w) = withForeignPtr renderer $ \ptr ->
                                            intToBool (-1) (fmap fromCInt $ gfxFilledEllipseColor ptr x y rx ry w)

foreign import ccall unsafe "pieColor" gfxPieColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

pie :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
pie renderer x y r s e (Pixel w) = withForeignPtr renderer $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxPieColor ptr x y r s e w)

foreign import ccall unsafe "filledPieColor" gfxFilledPieColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledPie :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledPie renderer x y r s e (Pixel w) = withForeignPtr renderer $ \ptr ->
                                        intToBool (-1) (fmap fromCInt $ gfxFilledPieColor ptr x y r s e w)

foreign import ccall unsafe "trigonColor" gfxTrigonColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

trigon :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
trigon renderer x1 y1 x2 y2 x3 y3 (Pixel w) = withForeignPtr renderer $ \ptr ->
                                             intToBool(-1) (fmap fromCInt $ gfxTrigonColor ptr x1 y1 x2 y2 x3 y3 w)

foreign import ccall unsafe "aatrigonColor" gfxAATrigonColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaTrigon :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaTrigon renderer x1 y1 x2 y2 x3 y3 (Pixel w) = withForeignPtr renderer $ \ptr ->
                                               intToBool(-1) (fmap fromCInt $ gfxAATrigonColor ptr x1 y1 x2 y2 x3 y3 w)

foreign import ccall unsafe "filledTrigonColor" gfxFilledTrigonColor :: Ptr RendererStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledTrigon :: Renderer -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledTrigon renderer x1 y1 x2 y2 x3 y3 (Pixel w) = withForeignPtr renderer $ \ptr ->
                                                   intToBool(-1) (fmap fromCInt $ gfxFilledTrigonColor ptr x1 y1 x2 y2 x3 y3 w)

foreign import ccall unsafe "polygonColor" gfxPolygonColor :: Ptr RendererStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Word32 -> IO CInt

polygon :: Renderer -> [(Int16, Int16)] -> Pixel ->  IO Bool
polygon renderer list (Pixel w) = withForeignPtr renderer $ \ptr ->
                                 withArray (map fst list) $ \xs ->
                                 withArray (map snd list) $ \ys ->
                                 intToBool (-1) $ fmap fromCInt $ gfxPolygonColor ptr xs ys (length list) w

foreign import ccall unsafe "aapolygonColor" gfxAAPolygonColor :: Ptr RendererStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Word32 -> IO CInt

aaPolygon :: Renderer -> [(Int16, Int16)] -> Pixel ->  IO Bool
aaPolygon renderer list (Pixel w) = withForeignPtr renderer $ \ptr ->
                                   withArray (map fst list) $ \xs ->
                                   withArray (map snd list) $ \ys ->
                                   intToBool (-1) $ fmap fromCInt $ gfxAAPolygonColor ptr xs ys (length list) w

foreign import ccall unsafe "filledPolygonColor" gfxFilledPolygonColor :: Ptr RendererStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Word32 -> IO CInt

filledPolygon :: Renderer -> [(Int16, Int16)] -> Pixel ->  IO Bool
filledPolygon renderer list (Pixel w) = withForeignPtr renderer $ \ptr ->
                                       withArray (map fst list) $ \xs ->
                                       withArray (map snd list) $ \ys ->
                                       intToBool (-1) $ fmap fromCInt $ gfxFilledPolygonColor ptr xs ys (length list) w

foreign import ccall unsafe "texturedPolygon" gfxTexturedPolygon :: Ptr RendererStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Ptr RendererStruct -> Int -> Int -> IO CInt

texturedPolygon :: Renderer -> [(Int16, Int16)] -> Renderer -> Int -> Int ->  IO Bool
texturedPolygon renderer list texture dx dy = withForeignPtr renderer $ \ptr ->
                                             withForeignPtr texture $ \txt ->
                                             withArray (map fst list) $ \xs ->
                                             withArray (map snd list) $ \ys ->
                                             intToBool (-1) $ fmap fromCInt $ gfxTexturedPolygon ptr xs ys (length list) txt dx dy

foreign import ccall unsafe "bezierColor" gfxBezierColor :: Ptr RendererStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Int -> Word32 -> IO CInt

bezier :: Renderer -> [(Int16, Int16)] -> Int -> Pixel ->  IO Bool
bezier renderer list steps (Pixel w) = withForeignPtr renderer $ \ptr ->
                                      withArray (map fst list) $ \xs ->
                                      withArray (map snd list) $ \ys ->
                                      intToBool (-1) $ fmap fromCInt $ gfxBezierColor ptr xs ys (length list) steps w


-- * Characters/Strings */
--
--     Int16 characterColor(SDL_Surface * dst, Sint16 x, Sint16 y, char c, Uint32 color);
--     Int16 characterRGBA(SDL_Surface * dst, Sint16 x, Sint16 y, char c, Uint8 r, Uint8 g, Uint8 b, Uint8 a);
--     Int16 stringColor(SDL_Surface * dst, Sint16 x, Sint16 y, const char *c, Uint32 color);
--     Int16 stringRGBA(SDL_Surface * dst, Sint16 x, Sint16 y, const char *c, Uint8 r, Uint8 g, Uint8 b, Uint8 a);
--
--     void gfxPrimitivesSetFont(const void *fontdata, Int16 cw, Int16 ch);
--
