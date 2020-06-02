import System.Environment -- command line arguments
import System.IO -- file operations

main :: IO()
main = do args <- getArgs
          putStrLn "hello, world!"
          let filename = head args
          readFromFile filename

-- consider adding "score::Float" field
data Ninja = Ninja { name:: String, country:: Char,
                     status:: String, exam1:: Float,
                     exam2:: Float, ability1:: String,
                     ability2:: String, r:: Int}
                     deriving Show

fire :: [Ninja] -- Land of Fire
fire = []

lightning :: [Ninja] -- Land of Lightning
lightning = []

water :: [Ninja] -- Land of Water
water = []

wind :: [Ninja] -- Land of Wind
wind = []

earth :: [Ninja] -- Land of Earth
earth = []


-- enumeration can be used instead
type Ability = (String, Float)
abilities :: [Ability]
abilities = [("Clone",20),
             ("Hit", 10),
             ("Lightning", 50),
             ("Vision", 30),
             ("Sand", 50),
             ("Fire", 40),
             ("Water", 30),
             ("Blade", 20),
             ("Summon", 50),
             ("Storm", 10),
             ("Rock", 20)]

getAbilityScore :: String -> Float
getAbilityScore str = getScore str abilities
            where
            getScore :: String -> [Ability] -> Float
            getScore s []    = 0
            getScore s xs@((a, score):xs')
                | a == s        = score
                | otherwise     = getScore s xs'

getScore :: Ninja -> Float
getScore a = 0.5 * (exam1 a) + 0.3 * (exam2 a) + getAbilityScore (ability1 a) + getAbilityScore (ability2 a)

readFromFile :: String -> IO()
readFromFile filename = do handle <- openFile filename ReadMode
                           readLoop handle
                           hClose handle

readLoop :: Handle -> IO()
readLoop handle = do eof <- hIsEOF handle
                     if eof
                        then return()
                        else do line <- hGetLine handle
                                let ninja = parseLine (words line)
                                -- let ninjas = placeList ninja exam
                                -- putStrLn (show(ninja))
                                -- putStrLn (show (getScore ninja))
                                readLoop handle

parseLine :: [String] -> Ninja
parseLine [n, c, e1, e2, a1, a2] = Ninja { name = n,
                        country = countryChar c, status = "Junior",
                        exam1 = read e1 :: Float, exam2 = read e2 :: Float,
                        ability1 = a1, ability2 = a2, r = 0}

countryChar :: String -> Char
countryChar c = case c of
    "Fire"      -> 'f'
    "Lightning" -> 'l'
    "Wind"      -> 'n'
    "Water"     -> 'w'
    "Earth"     -> 'e'

