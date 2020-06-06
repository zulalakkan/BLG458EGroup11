import System.Environment -- command line arguments
import System.IO -- file operations

main :: IO()
main = do args <- getArgs
          let filename = head args
          lands <- readFromFile filename
          menu lands
          return()

menu :: [[Ninja]] -> IO()
menu lands = do putStrLn "a) View a Country's Ninja Information\n\
                \b) View All Countries' Ninja Information\n\
                \c) Make a Round Between Ninjas\n\
                \d) Make a Round Between Countries\n\
                \e) Exit"
                putStr "Enter the action: "
                hFlush stdout                                          -- to make putStr and getLine work as expected
                str <- getLine
                if str == [] then menu lands
                else do let ch = head str
                        lands' <- action ch lands
                        putStrLn ""
                        if elem ch "eE" then return() 
                        else menu lands'

action :: Char -> [[Ninja]] -> IO [[Ninja]]
action ch ns
    | elem ch "aA" = do putStr "Enter the country code: "
                        hFlush stdout 
                        line <- getLine
                        actionA ns $ head line
                        return(ns)
    | elem ch "bB" = do printNinjas $ sort precede $ concat ns
                        return(ns)
    | elem ch "cC" = do actionC ns
                        return(ns)
    | elem ch "dD" = return(ns)
    | elem ch "eE" = return(ns)
    | otherwise    = do putStrLn "Unknown action!"
                        return (ns)

actionA :: [[Ninja]] -> (Char -> IO())
actionA ns = \ch -> do let land = (!!) ns $ index ch
                       printNinjas land
                       if promoted land then putStrLn $ warning ch else return()

actionC :: [[Ninja]] -> IO [[Ninja]]
actionC ns = do putStr "Enter first ninja's name: "
                hFlush stdout
                firstName <- getLine
                putStr "Enter first ninja's country code: "
                hFlush stdout
                firstCode <- getLine
                if True `elem` checkNinja (concat ns) firstName (firstCode !! 0)
                    then do putStr "Enter second ninja's name: "
                            hFlush stdout
                            secondName <- getLine
                            putStr "Enter second ninja's country code: "
                            hFlush stdout
                            secondCode <- getLine
                            if True `elem` checkNinja (concat ns) secondName (secondCode !! 0)
                                then do putStrLn "FIGHT!"
                                        putStrLn $ name ((fight (getNinja (concat ns) firstName (firstCode !! 0)) (getNinja (concat ns) secondName (secondCode !! 0))) !! 0)
                                        return(ns)
                                else do putStrLn "The given ninja doesn't exist"
                                        return(ns)
                    else do putStrLn "The given ninja doesn't exist!"
                            return(ns)

printNinjas :: [Ninja] -> IO()
printNinjas []     = return()
printNinjas (n:ns) = do putStrLn $ name n ++ ", Score: " ++ (show . getScore) n ++ ", Status: \
                        \" ++ status n ++ ", Round: "++ (show . r) n
                        printNinjas ns

-- This code is most probably super stupid (a list of bools for one ninja!), but so far it seems to work (written at 4AM).
-- If you have any idea how to simplify it please do so and make me aware of it.
checkNinja :: [Ninja] -> String -> Char -> [Bool]
checkNinja [] _ _ = False : []
checkNinja (n:ns) ninjaName ninjaCode = (((name n) == ninjaName) && ((country n) == ninjaCode)) : checkNinja ns ninjaName ninjaCode

getNinja :: [Ninja] -> String -> Char -> Ninja
getNinja ns ninjaName ninjaCode = (filter (\n -> ((name n) == ninjaName) && ((country n == ninjaCode))) ns) !! 0

fight :: Ninja -> Ninja -> [Ninja]
fight n1 n2
    |getScore n1 > getScore n2 = [n1, n2]
    |getScore n1 == getScore n2 = betterAbility n1 n2
        where betterAbility n1 n2
                |(((getAbilityScore $ ability1 n1) + (getAbilityScore $ ability2 n1)) > ((getAbilityScore $ ability1 n2) + (getAbilityScore $ ability2 n2))) = [n1, n2]
                |(((getAbilityScore $ ability1 n1) + (getAbilityScore $ ability2 n1)) == ((getAbilityScore $ ability1 n2) + (getAbilityScore $ ability2 n2))) = [n1, n2]
                |otherwise = [n2, n1]

lands :: [String]
lands = ["Fire", "Lightning", "Water", "Wind", "Earth"]

warning :: Char -> String
warning ch = lands !! (index ch) ++ " country cannot be included in a fight" 

promoted :: [Ninja] -> Bool
promoted [] = False
promoted n  = (status . last) n == "Journeyman"

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

participants :: [[Ninja]]
participants = [fire, lightning, water, wind, earth]

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
getAbilityScore str = abilityScore str abilities
            where
            abilityScore :: String -> [Ability] -> Float
            abilityScore s []    = 0
            abilityScore s xs@((a, score):xs')
                | s == a        = score
                | otherwise     = abilityScore s xs'

getScore :: Ninja -> Float
getScore a = 0.5 * (exam1 a) + 0.3 * (exam2 a) + (getAbilityScore . ability1) a + (getAbilityScore . ability2) a + 10.0 * read (show $ r a)::Float

readFromFile :: String -> IO [[Ninja]]
readFromFile filename = do handle <- openFile filename ReadMode
                           ninjas <- readLoop handle participants
                           hClose handle
                           return(ninjas)

readLoop :: Handle -> [[Ninja]] -> IO [[Ninja]]
readLoop handle participants = do 
                        eof <- hIsEOF handle
                        if eof
                        then return(participants)
                        else do line <- hGetLine handle
                                let ninja = parseLine $ words line
                                let participants' = placeNinja ninja participants
                                readLoop handle participants'

parseLine :: [String] -> Ninja
parseLine [n, c, e1, e2, a1, a2] = Ninja { name = n,
                        country = countryCode c, status = "Junior",
                        exam1 = read e1 :: Float, exam2 = read e2 :: Float,
                        ability1 = a1, ability2 = a2, r = 0}

insert :: (Ninja -> Ninja -> Bool) -> Ninja -> [Ninja] -> [Ninja]
insert f ninja [] = [ninja]
insert f ninja ns@(n':ns')
    | f ninja n' = ninja : ns
    | otherwise  = n' : insert f ninja ns' 

precede :: Ninja -> Ninja -> Bool
precede n1 n2
    | r n1 < r n2  = True
    | r n1 == r n2 = getScore n1 >= getScore n2
    | otherwise    = False 

sort :: (Ninja -> Ninja -> Bool) -> [Ninja] -> [Ninja]
sort f []     = []
sort f (n:ns) = insert f n $ sort f ns

placeNinja :: Ninja -> [[Ninja]] -> [[Ninja]]
placeNinja ninja lands = placeIter (index $ country ninja) lands 0
            where
                placeIter :: Int -> [[Ninja]] -> Int -> [[Ninja]]
                placeIter _ _ 5      = []
                placeIter i (l:ls) n = if n == i then (insert precede ninja l) : (ls)
                                            else l : (placeIter i ls (n+1))

index :: Char -> Int
index c
    | elem c "fF" = 0                       -- fire
    | elem c "lL" = 1                       -- lightning
    | elem c "wW" = 2                       -- water
    | elem c "nN" = 3                       -- wind
    | elem c "eE" = 4                       -- earth
    | otherwise   = error "unknown country character"

countryCode :: String -> Char
countryCode c = case c of
    "Fire"      -> 'f'
    "Lightning" -> 'l'
    "Wind"      -> 'n'
    "Water"     -> 'w'
    "Earth"     -> 'e'

update :: Ninja -> [[Ninja]] -> (Ninja->Ninja) -> [[Ninja]]
update ninja ninjas updateFunc = updateList (updateFunc ninja) ninjas 

updateRound :: Ninja -> Ninja
updateRound n = n {r = r n + 1}

updateStatus :: Ninja -> Ninja
updateStatus n = n {status = "Journeyman"}

updateList :: Ninja -> [[Ninja]] -> [[Ninja]]
updateList _ []         = []
updateList n ls@(l:ls') = if (index $ country n) + length ls == 5
                            then (updateLand n l):ls'
                            else l: updateList n ls'

-- update list with updated ninja 
-- consider ninja comparison instead of name comparison
-- check insert part again!! 
updateLand :: Ninja -> [Ninja] -> [Ninja]
updateLand _ []         = []
updateLand ninja (n:ns) = if (name ninja) == name n
                            then insert precede ninja ns
                            else n: updateLand ninja ns