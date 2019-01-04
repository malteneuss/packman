{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable
import Control.Monad
import System.IO
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as V

import Knapsack


main :: IO ()
main = do
  cmdOpts <- parseCmdLine
  -- read in instance
  if input cmdOpts == StdInput
  then putStrLn fileFormatHelpText
  else return ()
  packInstance <- mkInstance (input cmdOpts)
  case (input cmdOpts) of
    (GenRandInput _) -> do
      putStrLn "Generated instance: "
      TIO.putStr $ showItemDetails [0..(itemCount packInstance)-1]
        (values packInstance) (weights packInstance)
      putStrLn $ "Capacity: " ++ show (capacity packInstance)
      putStrLn ""
    _ -> return ()
  -- write instance to file is user wants it
  case (output cmdOpts) of
    (Just saveFile) -> withFile saveFile WriteMode (writeInstance packInstance)
    _ -> return ()
  -- print out solution
  let packSolution = solveInstance (solver cmdOpts) packInstance
  putStrLn $ case solver cmdOpts of
    Greedy -> "Greedy solution:"
    Optimal -> "Optimal solution:"
    FPTAS e -> "FPTAS solution with guaranteed max " ++ show (100*e)
      ++ "% deviation from optimal total value"
  if verboseSolution cmdOpts
  then TIO.putStrLn $ showVerboseSolution packInstance packSolution
  else do
    putStrLn $ show (bestItems packSolution)
    putStrLn $ "Total value: " ++ (show $ totalValue packSolution)
    putStrLn $ "Total weight: " ++ (show $ totalWeight packSolution)
  where
    mkInstance (FileInput f) = withFile f ReadMode parseInstance
    mkInstance (StdInput) = parseInstance stdin
    mkInstance (GenRandInput randItemCount) = genRandInstance randItemCount

parseInstance :: Handle -> IO PackInstance
parseInstance h = do
  fstLine <- readInts
  if length fstLine /= 2
  then error "First line must contain the itemCount and capacity numbers"
  else return ()
  let (itemCount:capacity:_) = fstLine
  values <- readVector
  weights <- readVector
  return $
    (case () of _ -- error reporting
                 | itemCount < 1 -> error "itemCount is negative, must be > 1"
                  | V.length values /= itemCount
                    -> error "number of values doesn't match itemCount"
                  | V.length weights /= itemCount
                    -> error "number of weights doesn't match itemCount"
                  | V.any (<0) values
                    -> error "values contain a negative number"
                  | V.any (<0) weights
                    -> error "weights contain a negative number"
                  | otherwise -> PackInstance {..})
  where
    readInts = fmap ((map (read . T.unpack)) . T.words)
      $ TIO.hGetLine h :: IO [Int]
    readVector = fmap V.fromList readInts :: IO (V.Vector Int)

writeInstance packInstance h = do
  hPutStrLn h $ (show $ itemCount packInstance)
    ++ " " ++ (show $ capacity packInstance)
  hPutStrLn h $ writeVector (values packInstance)
  hPutStrLn h $ writeVector (weights packInstance)
  where
    writeVector vec = V.foldl' (\str v -> str ++ show v ++ " ") "" vec

solveInstance :: Solver -> PackInstance -> PackSolution
solveInstance Greedy packInstance = packGreedy packInstance
solveInstance (FPTAS deviationFactor) packInstance
  = packFPTAS deviationFactor packInstance
solveInstance Optimal packInstance = packOptimal packInstance

genRandInstance :: Int -> IO PackInstance
genRandInstance itemCount = do
  -- let itemCountRange = (100,100)
  -- itemCount <- randomRIO itemCountRange
  let valueRange = (1, 10*itemCount)
  let weightRange = (1, 10*itemCount)
  let capacityRange = (1, 10*itemCount)
  values <- fmap V.fromList <$> replicateM itemCount $ randomRIO valueRange
  weights <- fmap V.fromList $ replicateM itemCount $ randomRIO weightRange
  capacity <- randomRIO capacityRange
  return $ PackInstance itemCount values capacity weights

showVerboseSolution :: PackInstance -> PackSolution -> T.Text
showVerboseSolution packInstance packSolution =
  (showItemDetails (V.toList (bestItems packSolution)) (values packInstance)
    (weights packInstance))
  `T.append` (T.pack $ "Total\t" ++ (show $ totalValue packSolution)
          ++ "\t" ++ (show $ totalWeight packSolution) )

showItemDetails :: [Int] -> V.Vector Int -> V.Vector Int -> T.Text
showItemDetails items values weights =
  T.unlines $ ("Item\tValue\tWeight") : map (\i ->
    let v = values V.! i
        w = weights V.! i
    in T.pack $ show i ++ "\t" ++ show v ++ "\t" ++ show w
    ) items




parseCmdLine :: IO CmdOpts
parseCmdLine = execParser $
  info (CmdOpts <$> solverSelect <*> inputSelect
    <*> verbosity <*> saveOutput <**> helper)
    ( fullDesc
  <> progDesc "FILE_FORMAT <empty>. Workaround: Try out --stdin to see a description of the FILE_FORMAT"
  <> header "packman - a 0/1 knapsack problem solver" )

data CmdOpts = CmdOpts
  { solver :: Solver
  , input :: Input
  , verboseSolution :: Bool
  , output :: Maybe FilePath
  } deriving Show


saveOutput :: Parser (Maybe FilePath)
saveOutput = optional $ strOption
   ( long "save"
  <> short 's'
  <> metavar "FILENAME"
  <> help ("Save the processed knapsack instance, e.g. when randomly generated, to the specified file.\n")
   )

solverSelect :: Parser Solver
solverSelect = maybe Greedy id <$> -- use Greedy solver if nothing selected
   (optional greedy <|> optional fptas <|> optional optimal)


data Solver
  = Greedy
  | FPTAS DeviationFactor
  | Optimal
  deriving Show

greedy :: Parser Solver
greedy = flag' Greedy
   ( long "greedy"
  <> help greedyHelpText
   )

fptas :: Parser Solver
fptas = FPTAS <$> option auto
   ( long "fptas"
  -- <> value 0.2
  -- <> showDefault -- not working
  <> metavar "DEVIATION_FACTOR"
  <> help fptasHelpText
   )

optimal :: Parser Solver
optimal = flag' Optimal
   ( long "optimal"
  <> help optimalHelpText
   )

greedyHelpText = "Use greedy solver. Fastest performance\
\ but in the worst case the solution has only half the value of the optimum."

fptasHelpText = "Use FPTAS solver with an\
\ deviation/approximation factor epsilon 'e' in (0,1].\
\ This factor represent how much percent the value of a solution is\
\ allowed to deviate from the optimum, e.g. a factor of 0.1 means 10%.\
\ Performance depends highly on the deviation factor,\
\ A smaller 'e' means slower performance but a better solution value.\
\ The value v of a solution is guaranteed to satisfy (1-e)*opt <= v <= opt."

optimalHelpText = "Use optimal solver. Very slow performance\
\ but the solution has an optimal value."

inputSelect :: Parser Input
inputSelect = fileInput <|> randomInput <|> stdInput

data Input
  = FileInput FilePath
  | StdInput
  | GenRandInput ItemCount
  deriving (Show, Eq)
type ItemCount = Int

fileInput :: Parser Input
fileInput = FileInput <$> strOption
   ( long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help ("Load a knapsack instance from the specified file.\n")
   )

randomInput :: Parser Input
randomInput = GenRandInput <$> option auto
  (  long "random"
  <> short 'r'
  <> metavar "ITEMCOUNT"
  <> help "Generate a random knapsack instance with the specified number of items"
  )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help ("Read a knapsack instance from stdin.\n")
  )

verbosity :: Parser Bool
verbosity = switch
  (  long "verbose"
  <> short 'v'
  <> help "Show details of items in the solution"
  )

fileFormatHelpText = "FILE_FORMAT:\n\
\<itemCount> <capacity>\n\
\<values>\n\
\<weights>\n\
\For example:\n\
\3 5\n\
\1 2 3\n\
\3 2 1\n\
\This instance has 3 items and capacity 5\n\
\Item 0 has value 1, item 1 has 2 and item 2 has 3\n\
\Item 0 has weight 3, item 1 has 2 and item 2 has 1\n\
\Note: There has to be a value and a weight for every item!"
