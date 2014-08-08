import qualified Test.Parser
import qualified Test.Tape
import qualified Test.Eval
import Test.Helper

main = defaultMain $ testGroup "Tests" tests
  where tests = [ Test.Parser.tests
                , Test.Tape.tests
                , Test.Eval.tests
                ]

