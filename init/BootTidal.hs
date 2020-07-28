:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""
:set -XDataKinds

import Sound.Tidal.Context

-- https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md
-- :def hoogle (\x -> return $ ":!/Users/xxx/Library/Haskell/bin/hoogle --info "++x)

:script ~/vis/vim-tidal/init/tidalScripts/Targets.hs
:script ~/vis/vim-tidal/init/tidalScripts/Transitions.hs
:script ~/vis/vim-tidal/init/tidalScripts/SuperDirtInterface.hs
:script ~/vis/vim-tidal/init/tidalScripts/Shorthands.hs
:script ~/vis/vim-tidal/init/tidalScripts/Func.hs
:script ~/vis/vim-tidal/init/tidalScripts/P5hs.hs

-- :script tidalScripts/Targets.hs
-- :script tidalScripts/Targets.hs
-- :script tidalScripts/Targets.hs

-- :script /path/to/here/tidal/stream/Stream.hs
-- :script /path/to/here/tidal/lib/Lib.hs
-- :script /path/to/here/tidal/osc/targets/TargetLibs.hs

:set prompt "tidal> "