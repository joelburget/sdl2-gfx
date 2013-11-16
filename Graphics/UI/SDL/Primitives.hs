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

foreign import ccall unsafe "pixelColor" gfxPixelColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Word32 -> IO CInt

pixel :: Surface -> Int16 -> Int16 -> Pixel -> IO Bool
pixel surface x y (Pixel w) = withForeignPtr surface $ \ptr ->
                              intToBool (-1) (fmap fromCInt $ gfxPixelColor ptr x y w)

foreign import ccall unsafe "hlineColor" gfxHLineColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

hLine :: Surface -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
hLine surface x1 x2 y (Pixel w) = withForeignPtr surface $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxHLineColor ptr x1 x2 y w)

foreign import ccall unsafe "vlineColor" gfxVLineColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

vLine :: Surface -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
vLine surface x y1 y2 (Pixel w) = withForeignPtr surface $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxVLineColor ptr x y1 y2 w)

foreign import ccall unsafe "rectangleColor" gfxRectangleColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

rectangle :: Surface -> Rect -> Pixel -> IO Bool
rectangle surface (Rect x y w h) (Pixel c) = withForeignPtr surface $ \ptr ->
                                             intToBool (-1) (fmap fromCInt $ gfxRectangleColor ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) c)

foreign import ccall unsafe "boxColor" gfxBoxColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

box :: Surface -> Rect -> Pixel -> IO Bool
box surface (Rect x y w h) (Pixel c) = withForeignPtr surface $ \ptr ->
                                       intToBool (-1) (fmap fromCInt $ gfxBoxColor ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) c)

foreign import ccall unsafe "lineColor" gfxLineColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

line :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
line surface x y x' y' (Pixel w) = withForeignPtr surface $ \ptr ->
                                   intToBool (-1) (fmap fromCInt $ gfxLineColor ptr x y x' y' w)

foreign import ccall unsafe "aalineColor" gfxAALineColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaLine :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaLine surface x y x' y' (Pixel w) = withForeignPtr surface $ \ptr ->
                                     intToBool (-1) (fmap fromCInt $ gfxAALineColor ptr x y x' y' w)

foreign import ccall unsafe "circleColor" gfxCircleColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

circle :: Surface -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
circle surface x y r (Pixel w) = withForeignPtr surface $ \ptr ->
                                 intToBool (-1) (fmap fromCInt $ gfxCircleColor ptr x y r w)

foreign import ccall unsafe "arcColor" gfxArcColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

arc :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
arc surface x y r s e (Pixel w) = withForeignPtr surface $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxArcColor ptr x y r s e w)

foreign import ccall unsafe "aacircleColor" gfxAACircleColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaCircle :: Surface -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaCircle surface x y r (Pixel w) = withForeignPtr surface $ \ptr ->
                                   intToBool (-1) (fmap fromCInt $ gfxAACircleColor ptr x y r w)

foreign import ccall unsafe "filledCircleColor" gfxFilledCircleColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledCircle :: Surface -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledCircle surface x y r (Pixel w) = withForeignPtr surface $ \ptr ->
                                       intToBool (-1) (fmap fromCInt $ gfxFilledCircleColor ptr x y r w)

foreign import ccall unsafe "ellipseColor" gfxEllipseColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

ellipse :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
ellipse surface x y rx ry (Pixel w) = withForeignPtr surface $ \ptr ->
                                      intToBool (-1) (fmap fromCInt $ gfxEllipseColor ptr x y rx ry w)

foreign import ccall unsafe "aaellipseColor" gfxAAEllipseColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaEllipse :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaEllipse surface x y rx ry (Pixel w) = withForeignPtr surface $ \ptr ->
                                        intToBool (-1) (fmap fromCInt $ gfxAAEllipseColor ptr x y rx ry w)

foreign import ccall unsafe "filledEllipseColor" gfxFilledEllipseColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledEllipse :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledEllipse surface x y rx ry (Pixel w) = withForeignPtr surface $ \ptr ->
                                            intToBool (-1) (fmap fromCInt $ gfxFilledEllipseColor ptr x y rx ry w)

foreign import ccall unsafe "pieColor" gfxPieColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

pie :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
pie surface x y r s e (Pixel w) = withForeignPtr surface $ \ptr ->
                                  intToBool (-1) (fmap fromCInt $ gfxPieColor ptr x y r s e w)

foreign import ccall unsafe "filledPieColor" gfxFilledPieColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledPie :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledPie surface x y r s e (Pixel w) = withForeignPtr surface $ \ptr ->
                                        intToBool (-1) (fmap fromCInt $ gfxFilledPieColor ptr x y r s e w)

foreign import ccall unsafe "trigonColor" gfxTrigonColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

trigon :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
trigon surface x1 y1 x2 y2 x3 y3 (Pixel w) = withForeignPtr surface $ \ptr ->
                                             intToBool(-1) (fmap fromCInt $ gfxTrigonColor ptr x1 y1 x2 y2 x3 y3 w)

foreign import ccall unsafe "aatrigonColor" gfxAATrigonColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

aaTrigon :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
aaTrigon surface x1 y1 x2 y2 x3 y3 (Pixel w) = withForeignPtr surface $ \ptr ->
                                               intToBool(-1) (fmap fromCInt $ gfxAATrigonColor ptr x1 y1 x2 y2 x3 y3 w)

foreign import ccall unsafe "filledTrigonColor" gfxFilledTrigonColor :: Ptr SurfaceStruct -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Word32 -> IO CInt

filledTrigon :: Surface -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Pixel -> IO Bool
filledTrigon surface x1 y1 x2 y2 x3 y3 (Pixel w) = withForeignPtr surface $ \ptr ->
                                                   intToBool(-1) (fmap fromCInt $ gfxFilledTrigonColor ptr x1 y1 x2 y2 x3 y3 w)

foreign import ccall unsafe "polygonColor" gfxPolygonColor :: Ptr SurfaceStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Word32 -> IO CInt

polygon :: Surface -> [(Int16, Int16)] -> Pixel ->  IO Bool
polygon surface list (Pixel w) = withForeignPtr surface $ \ptr ->
                                 withArray (map fst list) $ \xs ->
                                 withArray (map snd list) $ \ys ->
                                 intToBool (-1) $ fmap fromCInt $ gfxPolygonColor ptr xs ys (length list) w

foreign import ccall unsafe "aapolygonColor" gfxAAPolygonColor :: Ptr SurfaceStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Word32 -> IO CInt

aaPolygon :: Surface -> [(Int16, Int16)] -> Pixel ->  IO Bool
aaPolygon surface list (Pixel w) = withForeignPtr surface $ \ptr ->
                                   withArray (map fst list) $ \xs ->
                                   withArray (map snd list) $ \ys ->
                                   intToBool (-1) $ fmap fromCInt $ gfxAAPolygonColor ptr xs ys (length list) w

foreign import ccall unsafe "filledPolygonColor" gfxFilledPolygonColor :: Ptr SurfaceStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Word32 -> IO CInt

filledPolygon :: Surface -> [(Int16, Int16)] -> Pixel ->  IO Bool
filledPolygon surface list (Pixel w) = withForeignPtr surface $ \ptr ->
                                       withArray (map fst list) $ \xs ->
                                       withArray (map snd list) $ \ys ->
                                       intToBool (-1) $ fmap fromCInt $ gfxFilledPolygonColor ptr xs ys (length list) w

foreign import ccall unsafe "texturedPolygon" gfxTexturedPolygon :: Ptr SurfaceStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Ptr SurfaceStruct -> Int -> Int -> IO CInt

texturedPolygon :: Surface -> [(Int16, Int16)] -> Surface -> Int -> Int ->  IO Bool
texturedPolygon surface list texture dx dy = withForeignPtr surface $ \ptr ->
                                             withForeignPtr texture $ \txt ->
                                             withArray (map fst list) $ \xs ->
                                             withArray (map snd list) $ \ys ->
                                             intToBool (-1) $ fmap fromCInt $ gfxTexturedPolygon ptr xs ys (length list) txt dx dy

foreign import ccall unsafe "bezierColor" gfxBezierColor :: Ptr SurfaceStruct -> Ptr Int16 -> Ptr Int16 -> Int -> Int -> Word32 -> IO CInt

bezier :: Surface -> [(Int16, Int16)] -> Int -> Pixel ->  IO Bool
bezier surface list steps (Pixel w) = withForeignPtr surface $ \ptr ->
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
