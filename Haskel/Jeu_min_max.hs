{-# OPTIONS_GHC -fno-warn-tabs #-}

-- # Déclaration du module à mettre au début du fichier
module Main where
	
-- # Importations
import System.Environment(getExecutablePath)
import System.FilePath(takeDirectory,(</>))
import System.Directory(doesFileExist)
import System.Random(randomRIO)
import System.IO(hFlush,stdout)
import Data.Char(isSpace,isDigit)

-- # Fonctions utilitaires
	-- ## Fonctions élémentaires
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splitOnComma :: String -> [String]
splitOnComma "" = [""]
splitOnComma txt = case break (==',') txt of
	(a,[])   -> [a]
	(a,_:bs) -> a : splitOnComma bs

	-- ## Lecture d'un argument de commande
readInt :: String -> Int -> Int -> IO Int
readInt prompt minimumLimit maximumLimit = loop where
	loop = do
		putStr prompt >> hFlush stdout
		line <- fmap trim getLine
		if all isDigit line && not (null line)
			then let valeur = read line in
				if valeur>=minimumLimit && valeur<=maximumLimit
					then return valeur
					else retry
			else retry
	retry =	putStrLn ("\nVeuillez entrer un entier entre " ++ show minimumLimit ++ " et " ++ show maximumLimit ++ ".") >> loop

	-- ## Chemin du fichier d’options
optionsPath :: IO FilePath
optionsPath = return "minmax_options.txt"

	-- ## Chargement des options sauvegardées
loadOptions :: IO [Int]
loadOptions = do
	path <- optionsPath
	exists <- doesFileExist path
	if exists
		then do
			content <- readFile path
			let vals = map read (splitOnComma content)
			if length vals==3 then return vals else return [1,100,5]
		else return [1,100,5]

	-- ## Sauvegarde des options
saveOptions :: Int -> Int -> Int -> IO ()
saveOptions minimumLimit maximumLimit turns = do
	path <- optionsPath
	writeFile path (show minimumLimit ++ "," ++ show maximumLimit ++ "," ++ show turns)

-- # Fonctions principales
	-- Affiche le menu principal
displayMainMenu :: IO ()
displayMainMenu = putStrLn "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter"

	-- Affiche le menu d'options
displayOptionsMenu :: Int -> Int -> Int -> IO ()
displayOptionsMenu minimumLimit maximumLimit turns =
	putStrLn $ "\n#### Menu options ####\n1 : Choisir les limites (actuellement : "
		++ show minimumLimit ++ " - " ++ show maximumLimit
		++ ")\n2 : Nombre de tours max (actuellement : " ++ show turns ++ ")"

	-- Joue une partie
playGame :: Int -> Int -> Int -> IO ()
playGame minimumLimit maximumLimit turns = do
	target <- randomRIO (minimumLimit, maximumLimit)
	putStrLn $ "\nTrouvez entre " ++ show minimumLimit ++ " et " ++ show maximumLimit ++ " en " ++ show turns ++ " tours"
	let loop attempt
		| attempt>turns = putStrLn $ "\nPerdu ! La réppnse était " ++ show target ++ "."
		| otherwise = do
			choice <- readInt ("Tour " ++ show attempt ++ " : ") minimumLimit maximumLimit
			if choice==target
				then putStrLn "\nGagné !"
				else do
					putStrLn $ if choice>target then "-" else "+"
					loop (attempt+1)
	loop 1

	-- Gère les options
manageOptions :: Int -> Int -> Int -> IO [Int]
manageOptions minimumLimit maximumLimit turns = do
	displayOptionsMenu minimumLimit maximumLimit turns
	choice <- readInt "? = " 1 2
	if choice==1
		then do
			newMin <- readInt "\nMin = " 1 maximumLimit
			newMax <- readInt "\nMax = " newMin 10000
			save newMin newMax turns
		else do
			newTurns <- readInt "\nTours max = " 1 100
			save minimumLimit maximumLimit newTurns
	where
		save a b c = saveOptions a b c >> return [a,b,c]

-- # Main
main :: IO ()
main = do
	opts@[minimumLimit, maximumLimit, turns] <- loadOptions
	let loop current@[curMin,curMax,curTurns] = do
		displayMainMenu
		menuChoice <- readInt "? = " 1 3
		case menuChoice of
			1 -> playGame curMin curMax curTurns >> loop current
			2 -> manageOptions curMin curMax curTurns >>= loop
			_ -> return ()
	loop opts
