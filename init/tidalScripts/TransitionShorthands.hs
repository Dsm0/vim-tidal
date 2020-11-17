 :{
 let bindTransition transition streamNum time stream p = transition streamNum time $ p
     -- lets you evaluate transitions before streamreplaces 
     -- for example:
     -- _jumpIn = bindTransition jumpIn
     -- will let you evaluate
     -- _jumpIn 1 1 d1 $ "808bd*4" & gain 1.3
     --
     -- this is useful because you can bind keys to add 
     xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
     xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
     histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
     wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
     waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
     jump i = transition tidal True (Sound.Tidal.Transition.jump) i
     jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
     jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
     jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
     mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
     interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
     interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
     clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
     clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
     anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
     anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
     forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
     d1 = p 1 . (|< orbit 0)
     d2 = p 2 . (|< orbit 1)
     d3 = p 3 . (|< orbit 2)
     d4 = p 4 . (|< orbit 3)
     d5 = p 5 . (|< orbit 4)
     d6 = p 6 . (|< orbit 5)
     d7 = p 7 . (|< orbit 6)
     d8 = p 8 . (|< orbit 7)
     d9 = p 9 . (|< orbit 8)
     d10 = p 10 . (|< orbit 9)
     d11 = p 11 . (|< orbit 10)
     d12 = p 12 . (|< orbit 11)
     d13 = p 13
     d14 = p 14
     d15 = p 15
     d16 = p 16
 :}


:{
let _jumpIn = bindTransition jumpIn

--     jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i

  
:}

let bindTransition transition time stream streamNum p = transition streamNum time $ p

let _jumpIn = bindTransition jumpIn -- 1 p 1

:t p 1 . (|< orbit 1)

let d1_ = p 1
let d1_ = p 1 . (|< orbit 1)
:t d1_

_jumpIn 1 p 1 . (|< orbit 1) $ "808bd*4" & gain 1.3
_jumpIn 1 d1_ $ "808bd*4" & gain 1.3

_jumpIn 1 

d1 $ "808bd*4" & gain 1.3


:t d1_
:t p 1

_jumpIn 1 p 1 $ "808bd*4" & gain 1.3
