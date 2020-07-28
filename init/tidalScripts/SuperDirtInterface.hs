import qualified Data.Map as Map_

:{
let changeFunc' stream list = sendFunc' list
      where toEvent' ws we ps pe v = Event (Sound.Tidal.Context.Context []) (Just $ Sound.Tidal.Context.Arc ws we) (Sound.Tidal.Context.Arc ps pe) v
              -- where [ws',we',ps',pe'] = map toRational [ws,we,ps,pe]
            makeFakeMap list_ = Map_.fromList list_
            makeFuncHelp :: [(String,Value)] -> ControlPattern
            makeFuncHelp y = Pattern $ fakeEvent (makeFakeMap y:: ControlMap)
              where fakeEvent a notARealArgument = [(toEvent' 0 1 0 1) a]
            makeFunc :: [(String,Value)] -> [ControlPattern]
            makeFunc x = [makeFuncHelp x]
            sendFunc' = mapM_ (streamFirst stream) . makeFunc
    changeFunc stream func newFunction = changeFunc' stream list
      where list = [(func, VS (render newFunction))]
    resetFunc stream func = changeFunc stream func (makeJSVar "")
    makeDraw stream newFunction = changeFunc stream "draw" newFunction
    makeSetup stream newFunction = changeFunc stream "setup" newFunction
    makeLoad stream newFunction = changeFunc stream "load" newFunction
      -- where list = [("scMessage",VS "loadSoundFiles"),("filePath",VS path)]
:}

:{
let changeFunc' stream list = sendFunc' list
      where toEvent' ws we ps pe v = Event (Sound.Tidal.Context.Context []) (Just $ Sound.Tidal.Context.Arc ws we) (Sound.Tidal.Context.Arc ps pe) v
              -- where [ws',we',ps',pe'] = map toRational [ws,we,ps,pe]
            makeFakeMap list_ = Map_.fromList list_
            makeFuncHelp :: [(String,Value)] -> ControlPattern
            makeFuncHelp y = Pattern $ fakeEvent (makeFakeMap y:: ControlMap)
              where fakeEvent a notARealArgument = [(toEvent' 0 1 0 1) a]
            makeFunc :: [(String,Value)] -> [ControlPattern]
            makeFunc x = [makeFuncHelp x]
            sendFunc' = mapM_ (streamFirst stream) . makeFunc
    loadSoundFiles' stream path = changeFunc' stream list
      where list = [("scMessage",VS "loadSoundFiles"),("filePath",VS path)]
    loadSynthDefs' stream path = changeFunc' stream list
      where list = [("scMessage",VS "loadSynthDefs"),("filePath",VS path)]
    loadOnly' stream path = changeFunc' stream list
      where list = [("scMessage",VS "loadOnly"),("filePath",VS path)]
    loadSoundFileFolder' stream path = changeFunc' stream list
      where list = [("scMessage",VS "loadSoundFileFolder"),("filePath",VS path)]
    loadSoundFile' stream path = changeFunc' stream list
      where list = [("scMessage",VS "loadSoundFile"),("filePath",VS path)]
    freeAllSoundFiles' stream = changeFunc' stream list
      where list = [("scMessage",VS "freeAllSoundFiles")]
    freeSoundFiles' stream name = changeFunc' stream list
      where list = [("scMessage",VS "freeSoundFiles"),("filePath",VS name)]
    postSampleInfo' stream = changeFunc' stream list
      where list = [("scMessage",VS "postSampleInfo")]
    initFreqSynthWindow' stream = changeFunc' stream list
      where list = [("scMessage",VS "initFreqSynthWindow")]
:}


:{
-- functions for sending paths to load into superDirt
let loadSoundFiles path = loadSoundFiles' tidal path 
    loadSynthDefs path = loadSynthDefs' tidal path
    loadOnly path = loadOnly' tidal path
    loadSoundFileFolder path = loadSoundFileFolder' tidal path 
    loadSoundFile path = loadSoundFile' tidal path
    freeAllSoundFiles = freeAllSoundFiles' tidal 
    freeSoundFiles name = freeSoundFiles' tidal name
    postSampleInfo = postSampleInfo' tidal
    initFreqSynthWindow = initFreqSynthWindow' tidal
:}

