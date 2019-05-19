type RGB = (Float, Float, Float)

type HSV = (Float, Float, Float)


main:: IO()
main = do   putStrLn "What\'s first color?"  
            firstColor <- getLine  
            putStrLn "What\'s second color?"  
            secondColor <- getLine
            putStrLn "What\'s the number of steps?"
            sNum <- getLine
            let stepNum = read sNum
            let rgb1 = (name2rgb(read firstColor))
            let rgb2 = (name2rgb(read secondColor))
            let hsv1 = (rgb2hsv rgb1)
            let hsv2 = (rgb2hsv rgb2)
            let grad = (hsvGradient hsv1 hsv2 stepNum)
            putStrLn (show grad)

rgb2hsv :: RGB -> HSV
rgb2hsv (r, g, b) = 
    if  diff == 0
    then (0,0, v)  else if rgbabs r == v 
        then ((checkh (bb -gg)), s, v) else if rgbabs g == v 
            then (checkh ((1 / 3) + rr - bb), s, v) else (checkh ((2 / 3) + gg - rr), s, v)
    where
        v = maximum (rgbabs r : rgbabs g : rgbabs b : [])
        diff = v - minimum (rgbabs r : rgbabs g : rgbabs b : [])
        rr = getdiff (rgbabs r)
        gg = getdiff (rgbabs g)
        bb = getdiff (rgbabs b)
        s =  diff / v
        getdiff :: Float -> Float
        getdiff d = (v - d) / 6 / diff +1 / 2
        checkh :: Float -> Float
        checkh h = if h < 0 then (h+1) * 360 else if h > 1 then (h-1) * 360 else h * 360
        rgbabs :: Float -> Float
        rgbabs x = x / 255.0
        
hsv2rgb :: HSV -> RGB
hsv2rgb (h, s, v) = 
    if s == 0 then ((v*255),(v*255),(v*255)) else getrgb i
    where
        h' = h / 60
        i = floor h'
        f = h' - fromIntegral (i)
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)
        getrgb :: Integer -> RGB
        getrgb i = 
                if mod i 6 == 0 then ((v*255), (t*255), (p*255))
                else if mod i 6 == 1 then ((q*255), (v*255), (p*255))
                else if mod i 6 == 2 then ((p*255), (v*255), (t*255))
                else if mod i 6 == 3 then ((p*255), (q*255), (v*255))
                else if mod i 6 == 4 then ((t*255), (p*255), (v*255))
                else ((v*255), (p*255), (q*255))
      
hsvGradient:: HSV -> HSV -> Integer -> [HSV]
hsvGradient (h1, s1, v1) (h2, s2, v2) 0 = []
hsvGradient (h1, s1, v1) (h2, s2, v2) i = gradIter (h1, s1, v1) (h2, s2, v2) i i
    where
        diffstep = (((h2-h1)/fromIntegral (i)),((s2-s1)/fromIntegral (i)),((v2-v1)/fromIntegral (i)))
        hsvSub :: HSV -> HSV -> HSV
        hsvSub (h1', s1', v1') (h2', s2', v2') = ((h2'-h1'), (s2'-s1'), (v2'-v1'))
        hsvAdd :: HSV -> HSV -> HSV
        hsvAdd (h1', s1', v1') (h2', s2', v2') = ((h2'+h1'), (s2'+s1'), (v2'+v1'))
        gradIter :: HSV -> HSV -> Integer -> Integer -> [HSV]
        gradIter (h1', s1', v1') (h2', s2', v2') num' iter
            |iter == 0      = [(h2', s2', v2')]
            |iter == num'   = [(h1', s1', v1')] ++ gradIter (hsvAdd diffstep (h1', s1', v1'))  (h2', s2', v2') num' (iter-1)
            |otherwise      = gradIter (h1', s1', v1') (hsvSub diffstep (h2', s2', v2')) num' (iter-1) ++ [(h2', s2', v2')]

hsv2desc :: HSV -> IO()
hsv2desc (h',s',v') = do 
                        let rev1 = hueName h'
                        let rev2 = saturationName s'
                        let rev3 = lightnessName v'
                        putStrLn rev1
                        putStrLn rev2
                        putStrLn rev3
    where
        hueName ::Float -> String
        hueName h = if h < 15 then "red"
        else if h == 15 then "reddish"
        else if h < 45  then"orange"
        else if h < 70  then"yellow"
        else if h < 79  then"lime"
        else if h < 163 then "green"
        else if h < 193 then "cyan"
        else if h < 240 then "blue"
        else if h < 260 then "indigo"
        else if h < 270 then "violet"
        else if h < 291 then "purple"
        else if h < 327 then "magenta"
        else if h < 344 then "rose"
        else "red"
        saturationName :: Float -> String
        saturationName s = if s < 3 then "grey"
        else if s < 10 then "almost grey"
        else if s < 30 then "very unsaturated"
        else if s < 46 then "unsaturated"
        else if s < 60 then "rather unsaturated"
        else if s < 80 then "saturated"
        else if s < 90 then "rather saturated"
        else "very saturated"
        lightnessName :: Float -> String
        lightnessName l = if l < 10 then "almost black"
        else if l < 22 then "very dark"
        else if l < 30 then "dark"
        else if l < 60 then "normal"
        else if l < 80 then "light"
        else if l < 94 then "very light"
        else "almost white"
        
data Name = Maroon|RebeccaPurple|DarkRed|Brown|Fuchsia|FireBrick|Crimson|Red|Tomato|Coral|IndianRed|LightCoral|DarkSalmon|Salmon|LightSalmon|OrangeRed|DarkOrange|Orange|Gold|DarkGoldenRod|GoldenRod|PaleGoldenRod|DarkKhaki|Khaki|Olive|Yellow|YellowGreen|DarkOliveGreen|OliveDrab|LawnGreen|ChartReuse|GreenYellow|DarkGreen|Green|ForestGreen|Lime|LimeGreen|LightGreen|PaleGreen|DarkSeaGreen|MediumSpringGreen|SpringGreen|SeaGreen|MediumAquaMarine|MediumSeaGreen|LightSeaGreen|DarkSlateGray|DarkSlateGrey|Teal|DarkCyan|Aqua|Cyan|LightCyan|DarkTurquoise|Turquoise|MediumTurquoise|PaleTurquoise|AquaMarine|PowderBlue|CadetBlue|SteelBlue|CornFlowerBlue|DeepSkyBlue|DodgerBlue|LightBlue|SkyBlue|LightSkyBlue|MidnightBlue|Navy|DarkBlue|MediumBlue|Blue|RoyalBlue|BlueViolet|Indigo|DarkSlateBlue|SlateBlue|MediumSlateBlue|MediumPurple|DarkMagenta|DarkViolet|DarkOrchid|MediumOrchid|Purple|Thistle|Plum|Violet|Magenta|Orchid|MediumVioletRed|PaleVioletRed|DeepPink|HotPink|LightPink|Pink|AntiqueWhite|Beige|Bisque|BlanchedAlmond|Wheat|CornSilk|LemonChiffon|LightGoldenRodYellow|LightYellow|SaddleBrown|Sienna|Chocolate|Peru|SandyBrown|BurlyWood|Tan|RosyBrown|Moccasin|NavajoWhite|PeachPuff|MistyRose|LavenderBlush|Linen|OldLace|PapayaWhip|SeaShell|MintCream|SlateGray|LightSlateGray|SlateGrey|LightSlateGrey|LightSteelBlue|Lavender|FloralWhite|AliceBlue|GhostWhite|HoneyDew|Ivory|Azure|Snow|Black|DimGray|Gray|DarkGray|DimGrey|Grey|DarkGrey|Silver|LightGray|LightGrey|Gainsboro|WhiteSmoke|White
    deriving (Read, Show)

name2rgb :: Name -> RGB
name2rgb name = case name of
    Fuchsia     ->  (255,0,255)
    Maroon      ->  (128,0,0)
    DarkRed     ->  (139,0,0)
    Brown       ->  (165,42,42)
    FireBrick   ->  (178,34,34)
    Crimson     ->  (220,20,60)
    Red         ->  (255,0,0)
    Tomato      ->  (255,99,71)
    Coral       ->  (255,127,80)
    IndianRed   ->  (205,92,92)
    LightCoral  ->  (240,128,128)
    DarkSalmon  ->  (233,150,122)
    Salmon      ->  (250,128,114)
    LightSalmon ->  (255,160,122)
    OrangeRed   ->  (255,69,0)
    DarkOrange  ->  (255,140,0)
    Orange      ->  (255,165,0)
    Gold        ->  (255,215,0)
    DarkGoldenRod     ->  (184,134,11)
    GoldenRod   ->  (218,165,32)
    PaleGoldenRod     ->  (238,232,170)
    DarkKhaki   ->  (189,183,107)
    Khaki       ->  (240,230,140)
    Olive       ->  (128,128,0)
    Yellow      ->  (255,255,0)
    YellowGreen    ->  (154,205,50)
    DarkOliveGreen    ->  (85,107,47)
    OliveDrab   ->  (107,142,35)
    LawnGreen   ->  (124,252,0)
    ChartReuse  ->  (127,255,0)
    GreenYellow    ->  (173,255,47)
    DarkGreen   ->  (0,100,0)
    Green       ->  (0,128,0)
    ForestGreen    ->  (34,139,34)
    Lime        ->  (0,255,0)
    LimeGreen   ->  (50,205,50)
    LightGreen  ->  (144,238,144)
    PaleGreen   ->  (152,251,152)
    DarkSeaGreen  ->  (143,188,143)
    MediumSpringGreen ->  (0,250,154)
    SpringGreen    ->  (0,255,127)
    SeaGreen    ->  (46,139,87)
    MediumAquaMarine  ->  (102,205,170)
    MediumSeaGreen    ->  (60,179,113)
    LightSeaGreen ->  (32,178,170)
    DarkSlateGray ->  (47,79,79)
    DarkSlateGrey ->  (47,79,79)
    Teal        ->  (0,128,128)
    DarkCyan    ->  (0,139,139)
    Aqua        ->  (0,255,255)
    Cyan        ->  (0,255,255)
    LightCyan   ->  (224,255,255)
    DarkTurquoise  ->  (0,206,209)
    Turquoise   ->  (64,224,208)
    MediumTurquoise    ->  (72,209,204)
    PaleTurquoise  ->  (175,238,238)
    AquaMarine  ->  (127,255,212)
    PowderBlue  ->  (176,224,230)
    CadetBlue   ->  (95,158,160)
    SteelBlue   ->  (70,130,180)
    CornFlowerBlue    ->  (100,149,237)
    DeepSkyBlue ->  (0,191,255)
    DodgerBlue  ->  (30,144,255)
    LightBlue   ->  (173,216,230)
    SkyBlue     ->  (135,206,235)
    LightSkyBlue  ->  (135,206,250)
    MidnightBlue   ->  (25,25,112)
    Navy        ->  (0,0,128)
    DarkBlue    ->  (0,0,139)
    MediumBlue  ->  (0,0,205)
    Blue        ->  (0,0,255)
    RoyalBlue   ->  (65,105,225)
    BlueViolet  ->  (138,43,226)
    Indigo      ->  (75,0,130)
    DarkSlateBlue ->  (72,61,139)
    SlateBlue   ->  (106,90,205)
    MediumSlateBlue   ->  (123,104,238)
    MediumPurple   ->  (147,112,219)
    DarkMagenta    ->  (139,0,139)
    DarkViolet  ->  (148,0,211)
    DarkOrchid  ->  (153,50,204)
    MediumOrchid   ->  (186,85,211)
    Purple      ->  (128,0,128)
    Thistle     ->  (216,191,216)
    Plum        ->  (221,160,221)
    Violet      ->  (238,130,238)
    Magenta     ->  (255,0,255)
    Orchid      ->  (218,112,214)
    MediumVioletRed ->  (199,21,133)
    PaleVioletRed   ->  (219,112,147)
    DeepPink    ->  (255,20,147)
    HotPink     ->  (255,105,180)
    LightPink   ->  (255,182,193)
    Pink        ->  (255,192,203)
    AntiqueWhite   ->  (250,235,215)
    Beige       ->  (245,245,220)
    Bisque      ->  (255,228,196)
    BlanchedAlmond ->  (255,235,205)
    Wheat       ->  (245,222,179)
    CornSilk    ->  (255,248,220)
    LemonChiffon   ->  (255,250,205)
    LightGoldenRodYellow ->  (250,250,210)
    LightYellow ->  (255,255,224)
    SaddleBrown ->  (139,69,19)
    Sienna      ->  (160,82,45)
    Chocolate   ->  (210,105,30)
    Peru        ->  (205,133,63)
    SandyBrown  ->  (244,164,96)
    BurlyWood   ->  (222,184,135)
    Tan         ->  (210,180,140)
    RosyBrown   ->  (188,143,143)
    Moccasin    ->  (255,228,181)
    NavajoWhite ->  (255,222,173)
    PeachPuff   ->  (255,218,185)
    MistyRose   ->  (255,228,225)
    LavenderBlush  ->  (255,240,245)
    Linen       ->  (250,240,230)
    OldLace     ->  (253,245,230)
    PapayaWhip  ->  (255,239,213)
    SeaShell    ->  (255,245,238)
    MintCream   ->  (245,255,250)
    SlateGray   ->  (112,128,144)
    SlateGrey   ->  (112,128,144)
    LightSlateGray    ->  (119,136,153)
    LightSlateGrey    ->  (119,136,153)
    LightSteelBlue    ->  (176,196,222)
    Lavender    ->  (230,230,250)
    FloralWhite ->  (255,250,240)
    AliceBlue   ->  (240,248,255)
    GhostWhite  ->  (248,248,255)
    HoneyDew    ->  (240,255,240)
    Ivory       ->  (255,255,240)
    RebeccaPurple -> (102,51,153)
    Azure       ->  (240,255,255)
    Snow        ->  (255,250,250)
    Black       ->  (0,0,0)
    DimGray     ->  (105,105,105)
    DimGrey     ->  (105,105,105)
    Gray        ->  (128,128,128)
    Grey        ->  (128,128,128)
    DarkGray    -> (169,169,169)
    DarkGrey    -> (169,169,169)
    Silver      ->  (192,192,192)
    LightGray   ->  (211,211,211)
    LightGrey   ->  (211,211,211)
    Gainsboro   ->  (220,220,220)
    WhiteSmoke  ->  (245,245,245)
    White       ->  (255,255,255)
