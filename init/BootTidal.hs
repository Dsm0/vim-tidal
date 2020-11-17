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
--
-- 
--

:{
do
  loadSoundFiles "john"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/games/halflife/apache"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/games/halflife/gman"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/games/halflife/tentacle"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/games/halflife/player"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/tidalSamples/songSamp/mord_fustang_kit"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/tidalSamples/customSamples/rezzett"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/KrazyRemi/SAMMY DRUMS/KICKS"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/KrazyRemi/all_snares_claps_5"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/tidalSamples/findsDig1/mlazer"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/tidalSamples/games/crossniq"
  loadSoundFiles "/media/ick/5EB068D0139EDA18/tidalSamples/games/gta"
  bpm 85
:}
:{
let distVerb = orbit 0
    compEnhance = orbit 2
    bitShape = orbit 4
    delay = orbit 6
    ampVerb = orbit 8
    lowCut = orbit 10
    punch = orbit 12
    plain = orbit 14
:}




:set prompt "tidal> "



