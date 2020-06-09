import System.Environment -- command line arguments
import System.IO -- file operations
import Data.Char (toLower, toUpper)

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
    | elem ch "aA" = do actionA ns
                        return(ns)
    | elem ch "bB" = do printNinjas $ sort precede $ concat ns
                        return(ns)
    | elem ch "cC" = do ns' <- actionC ns
                        return(ns')
    | elem ch "dD" = do ns' <- actionD ns
                        return(ns')
    | elem ch "eE" = do printJourneymans $concat ns
                        return(ns)
    | otherwise    = do putStrLn "Unknown action!"
                        return (ns)

actionA :: [[Ninja]] -> IO()
actionA ns = do putStr "Enter the country code: "
                hFlush stdout 
                line' <- getLine
                let line = toLowerString line'
                if null line then return() else do
                       let land = (!!) ns $ index $ head line
                       if null land then putStrLn "All ninjas from this country have been disqualified." else printNinjas land
                       if promoted land then putStrLn $ warning (head line) else return()

actionC :: [[Ninja]] -> IO [[Ninja]]
actionC ns = do fighters <- inputC ns
                if null fighters then return(ns)
                else do
                    let fightCondition = checkFightCondition (fighters !! 0) (fighters !! 1)
                    if fst fightCondition
                    then do let [winner,loser] = fight (fighters !! 0) (fighters !! 1)
                            let ninjas' = removeNinja loser (update winner ns)
                            printWinner $ getNinja (getLand (country winner) ninjas') (name winner)
                            return(ninjas')
                    else do putStrLn $ snd fightCondition
                            return(ns)

order = ["first", "second"]

inputC :: [[Ninja]] -> IO [Ninja]
inputC = inputLoop [] 0
    where
        inputLoop :: [Ninja] -> Int -> [[Ninja]] -> IO [Ninja]
        inputLoop res 2 _ = return (res)
        inputLoop res i ns = do 
                putStr $ "Enter " ++ (order !! i) ++ " ninja's name: "
                hFlush stdout
                line <- getLine
                if null line
                then do putStrLn "Empty name!"
                        return([])
                else do let name = toLowerString line
                        putStr $ "Enter " ++ (order !! i) ++ " ninja's country code: "
                        hFlush stdout
                        line <- getLine
                        if null line || (-1 == (index . head) line) 
                        then do putStrLn "Unknown country code!"
                                return ([])
                        else do let code = head line
                                if fst $ checkLand $ getLand code ns
                                then do if checkNinjaInLand (getLand code ns) name
                                        then do res'<- inputLoop ((getNinja (getLand code ns) name):res) (i+1) ns
                                                return(res')
                                        else do putStrLn "The given ninja doesn't exist!"
                                                return ([])
                                else do putStrLn $ snd $ checkLand $ getLand code ns
                                        return ([])


actionD :: [[Ninja]] -> IO [[Ninja]]
actionD ns = do putStr "Enter the first country code: "
                hFlush stdout
                firstCode <- getLine               
                if True == fst (checkLand (ns !! (index (head firstCode))))
                    then do putStr "Enter the second country code: "
                            hFlush stdout
                            secondCode <- getLine
                            if True == fst (checkLand (ns !! (index (head secondCode))))
                                then do if (index (head firstCode)) /= (index (head secondCode))
                                            then do let fightCondition = checkFightCondition ((ns !! (index $ head firstCode)) !! 0) ((ns !! (index $ head secondCode)) !! 0)
                                                    if True == fst fightCondition
                                                        then do let [winner, loser] = fight ((ns !! (index $ head firstCode)) !! 0) ((ns !! (index $ head secondCode)) !! 0)
                                                                let ninjas' = removeNinja loser (update winner ns)
                                                                printWinner $ getNinja (getLand (country winner) ninjas') (name winner)
                                                                return(ninjas')
                                                        else do putStrLn $ snd fightCondition
                                                                return(ns)
                                            else do putStrLn "A country can't fight itself."
                                                    return(ns)
                                else do putStrLn (snd (checkLand (ns !! (index (head secondCode)))))
                                        return(ns)
                    else do putStrLn (snd (checkLand (ns !! (index (head firstCode)))))
                            return(ns)

printNinjas :: [Ninja] -> IO()
printNinjas []     = return()
printNinjas (n:ns) = do print n
                        printNinjas ns

printJourneymans :: [Ninja] -> IO()
printJourneymans = (printNinjas . filter (\n -> status n == "Journeyman"))

printWinner :: Ninja -> IO()
printWinner n =  putStrLn $ "Winner: " ++name n ++ ", Round: "++ (show . r) n ++ ", Status: " ++ status n

getLand :: Char -> [[Ninja]] -> [Ninja]
getLand ch ns = (!!) ns $ index ch

checkNinjaInLand :: [Ninja] -> String -> Bool
checkNinjaInLand n ninjaName = filter (\x -> name x == ninjaName) n /= []

getNinja :: [Ninja] -> String -> Ninja
getNinja ns ninjaName = head (filter (\n -> name n == ninjaName) ns)

checkFightCondition :: Ninja -> Ninja -> (Bool, String)
checkFightCondition n1 n2
    |country n1 == country n2 = (False, "Ninjas from the same country can't fight!")
    |n1 == n2 = (False, "Ninja can't fight himself")
    |(status n1 == "Journeyman" || status n2 == "Journeyman") = (False, "Countries which have journeymans can't fight!")
    |otherwise = (True, "")

checkLand :: [Ninja] -> (Bool, String)
checkLand l
    |null l = (False, "All ninjas from this country have been disqualified.")
    |elem "Journeyman" (map status l) = (False, "Countries with journeymans can't fight.")
    |otherwise = (True, "")

fight :: Ninja -> Ninja -> [Ninja]
fight n1 n2
    |getScore n1 > getScore n2 = [n1, n2]
    |getScore n1 == getScore n2 = betterAbility n1 n2
    |otherwise = [n2, n1]
        where betterAbility n1 n2
                |(((getAbilityScore $ ability1 n1) + (getAbilityScore $ ability2 n1)) >= ((getAbilityScore $ ability1 n2) + (getAbilityScore $ ability2 n2))) = [n1, n2]
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
                     deriving (Eq)

instance Show Ninja where
    show n = name n ++ ", Score: " ++ (show . getScore) n ++ ", Status: \
                        \" ++ status n ++ ", Round: "++ (show . r) n

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

removeNinja :: Ninja -> [[Ninja]] -> [[Ninja]] 
removeNinja ninja = map (\x -> if x /= [] && (country ninja == (country . head) x) then remove ninja x else x)

remove :: Ninja -> [Ninja] -> [Ninja]
remove _ [] = []
remove n ns = [i | i <-ns, i /= n]

index :: Char -> Int
index c
    | elem c "fF" = 0                       -- fire
    | elem c "lL" = 1                       -- lightning
    | elem c "wW" = 2                       -- water
    | elem c "nN" = 3                       -- wind
    | elem c "eE" = 4                       -- earth
    | otherwise   = -1 -- error "unknown country character"

countryCode :: String -> Char
countryCode c = case c of
    "Fire"      -> 'f'
    "Lightning" -> 'l'
    "Wind"      -> 'n'
    "Water"     -> 'w'
    "Earth"     -> 'e'

update :: Ninja -> [[Ninja]] -> [[Ninja]]
update ninja = updateList (updateRound ninja)

updateRound :: Ninja -> Ninja
updateRound n = if r n == 2 then updateStatus n {r = r n + 1} else n {r = r n + 1}

updateStatus :: Ninja -> Ninja
updateStatus n = n {status = "Journeyman"}

updateList :: Ninja -> [[Ninja]] -> [[Ninja]]
updateList _ []         = []
updateList n ls@(l:ls')
    |(index $ country n) + length ls == 5 = (updateLand n l):ls'
    |otherwise = l: updateList n ls'

-- update list with updated ninja 
-- consider ninja comparison instead of name comparison
-- check insert part again!! 
updateLand :: Ninja -> [Ninja] -> [Ninja]
updateLand _ []         = []
updateLand ninja (n:ns)
    |(name ninja) == name n = insert precede ninja ns
    |otherwise = n: updateLand ninja ns
                        
toLowerString :: [Char] -> [Char]
toLowerString (x:xs) = toUpper x : map toLower xs
toLowerString [] = []

