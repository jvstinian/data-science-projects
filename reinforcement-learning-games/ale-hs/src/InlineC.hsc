{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module InlineC ( c_cos, faster_c_cos ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import           Foreign.C.Types

C.include "<math.h>"

c_cos :: CDouble -> IO CDouble
c_cos x = [C.exp| double { cos($(double x)) } |]

faster_c_cos :: CDouble -> IO CDouble
faster_c_cos x = [CU.exp| double { cos($(double x)) } |]

