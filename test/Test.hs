{-# LANGUAGE RecordWildCards #-}
import System.Exit
import System.Environment(getArgs)
import Test.QuickCheck

import qualified Data.Map as M
import TicTacToe

-- do extensive tests
extensive :: Args
extensive = Args {
              replay = Nothing,
              maxSuccess = 10000,
              maxDiscardRatio = 5,
              maxSize = 100,
              chatty = True
            }

-- do quick tests
quick :: Args
quick = Args {
          replay = Nothing,
          maxSuccess = 100,
          maxDiscardRatio = 5,
          maxSize = 100,
          chatty = True
        }











instance Arbitrary Position where
  arbitrary = elements [NW .. SE]

instance Arbitrary Board where
  arbitrary = listOf arbitrary >>= \poss -> return (Board [] (foldl (\m p -> M.insert p PlayerX m) M.empty poss))

testBoard = forAll arbitrary $ \board@(Board _ m) ->
              forAll arbitrary $ \pos ->
                if M.member pos m then True
                else (takeBack (move board pos)) == board

runTests :: [String] -> IO ()
runTests as =
  let runTestsAs args = quickCheckWithResult args testBoard >>= \res ->
                        case res of
                        Success {..} -> putStrLn "Success!" >> return ()
                        _ -> exitFailure
  in case as of
      ("-e":_) -> putStrLn "running extensive tests" >> runTestsAs extensive
      _ -> putStrLn "running quick tests" >> runTestsAs quick

main :: IO ()
main = getArgs >>= runTests
