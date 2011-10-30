import Test.Framework (defaultMain)

import qualified HStyle.Block.Tests (tests)

main :: IO ()
main = defaultMain
    [ HStyle.Block.Tests.tests
    ]
