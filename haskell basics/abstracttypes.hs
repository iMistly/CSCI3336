data Shape = Point
           | Circle Float
           | Rectangle Float Float
           | Square Float
           | Triangle Float Float
           | Composed Shape Shape

instance Show Shape where
    show Point = "."
    show (Circle r) = "Circle with radius " ++ show r
    show (Rectangle l w) = "Rectangle with " ++ show l ++ " long and " ++ show w ++ " wide"
    show (Square s) = "Square with side: " ++ show s
    show (Triangle b h) = "Triangle with base " ++ show b ++ " and height " ++ show h
    show (Composed s1 s2) = "Shape 1: " ++ show s1 ++ " and Shape 2: " ++ show s2

area :: Shape -> Float
area Point = 0
area (Circle r) = pi * r^2
area (Rectangle l w) = l * w
area (Square s) = s ^ 2
area (Triangle b h) = b * h / 2
area (Composed s1 s2) = area s1 + area s2


s1 :: Shape
s1 = Point

s2 = Circle 5.3

s3 = Rectangle 10 12.4

s4 = Square 5.7

s5 = Triangle 4 10.2

s6 = Composed s3 s5

areaC :: Shape -> Float
areaC s = case s of
            Point -> 0
            Circle r -> pi*r^2
            Rectangle l w -> l*w
            Square s -> s^2
            Triangle b h -> b*h*0.5
            Composed s1 s2 -> areaC s1 + areaC s2

getShape :: IO Shape
getShape = do
            putStrLn "Choose a shape: point, circle, rectangle, square, triangle, composed: "
            shapeType <- getLine
            case shapeType of
                "point" -> return Point
                "circle" -> do
                                putStrLn "Enter a radius: "
                                radius <- readLn
                                return (Circle radius)
                "rectangle" -> do
                                putStrLn "Enter length: "
                                length <- readLn
                                putStrLn "Enter width: "
                                width <- readLn
                                return (Rectangle length width)
                "square" -> return (Square 5)
                "triangle" -> return (Triangle 3 5)
                "composed" -> do
                                putStr "Shape 1: "
                                s1 <- getShape
                                putStr "Shape 2: "
                                s2 <- getShape
                                return (Composed s1 s2)
                _ -> getShape

main :: IO()
main = do
        s <- getShape 
        let a = area s
        print a