import Sound.Tidal.Context
-- import Control.Concurrent (threadDelay)
-- import Control.Concurrent.MVar (readMVar)
import qualified Control.Monad as CM
import qualified Sound.Tidal.Stream
import qualified Sound.Tidal.Tempo as T
import P5hs

-- used for sending custom calls to superdirt functions on a specific OSC path
:{
superdirtMessageOSC :: OSC
superdirtMessageOSC = OSC "/scMessage" $ Named {required = ["scMessage"]}
:}


:{
let p5Target :: Target
    p5Target = superdirtTarget {oName = "processing",oAddress = "192.168.1.128", oPort = 57130}
    p5OSC = OSC "/p5" $ Named {required = []}
:}

:{
let scMessageTarget :: Target
    scMessageTarget = Target {oName = "SuperDirt",
                          oAddress = "127.0.0.1",
                          oPort = 57120,
                          oLatency = 0.2,
                          oWindow = Nothing,
                          oSchedule = Pre BundleStamp
                         }
:}

import Data.List

:{
tidalTarget = superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}
:}

:{
tidal <- startStream (defaultConfig {cFrameTimespan = 1/20 , cTempoPort = 9611}) 
          [(tidalTarget,[superdirtShape,superdirtMessageOSC]),(p5Target,[p5OSC])]
:}

-- :{
-- glslViewerTarget :: Target
-- glslViewerTarget = superdirtTarget {oName = "glslViewer",
--                            oAddress = "127.0.0.1:/u_",
--                            oPort = 57140,
--                            oLatency = 0.02
--                             }
-- :}


:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetI tidal
    setB = streamSetB tidal
:}

