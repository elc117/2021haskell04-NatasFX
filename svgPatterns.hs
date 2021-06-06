import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
paletteRed :: Int -> [Float]
paletteRed n   = take n (cycle (replicate 90 255 ++ reverse [0,2.9..255] ++ replicate 90 0 ++ [0,2.9..255] ))

paletteGreen :: Int -> [Float]
paletteGreen n = take n (cycle ([0,2.9..255] ++ replicate 90 255 ++ reverse [0,2.9..255] ++ replicate 90 0))

paletteBlue :: Int -> [Float]
paletteBlue n  = take n (cycle (replicate 90 0 ++ [0,2.9..255] ++ replicate 90 255 ++ reverse [0,2.9..255]))

rgbPalette :: Int -> [(Float,Float,Float)]
rgbPalette n = zip3 (paletteBlue n) (paletteGreen n) (paletteRed n)

-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLineRev :: Int -> Float -> Float -> [Rect]
genRectsInLineRev n width gap = [((width - m*(w+gap)-gap-w, m*(h+gap)+gap), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)

genRectsInLine :: Int -> Float -> [Rect]
genRectsInLine n gap = [((m*(w+gap)+gap, m*(h+gap)+gap), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)

genRectsDiagonals :: Int -> Float -> Float -> [Rect]
genRectsDiagonals n width gap = genRectsInLineRev n width gap ++ genRectsInLine n gap

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String
svgRect ((x,y),w,h) = printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n"

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Float,Float,Float) -> String
svgStyle (r,g,b) = printf "fill:rgb(%.2f,%.2f,%.2f,.8); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main =
  writeFile "rects.svg" svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsDiagonals nrects w gap
        palette = rgbPalette (nrects*2)
        nrects = 720
        (w,h) = (500,500) -- width,height da imagem SVG
        gap = -49

