
{-main :: IO ()-}
{-main = do-}
  {-Test.Parser.tests-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck (Testable)

import Data.List
import Data.Ord

import qualified Test.Parser
import qualified Test.Tape
import qualified Test.Eval

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" qcProps

mkGroup :: Testable prop => TestName -> [(TestName, prop)] -> TestTree
mkGroup name tests = testGroup name $ map (uncurry QC.testProperty) tests

qcProps = [mkGroup "Parser" Test.Parser.properties
          ,mkGroup "Tape"   Test.Tape.properties
          ,mkGroup "Eval"   Test.Eval.properties
          ]
