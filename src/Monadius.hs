module Monadius (
  Monadius(..),
  initialMonadius,
  getVariables,
  GameVariables(..),
  shotButton,
  missileButton,
  powerUpButton,
  upButton,
  downButton,
  leftButton,
  rightButton,
  selfDestructButton
  ) where

import Data.Array ((!), Array(), array)
import Data.Complex
import Data.List
import Data.Maybe
import Graphics.UI.GLUT hiding (position, DebugMessage)

import Game
import Util

instance Game Monadius where
  update = updateMonadius
  render = renderMonadius
  isGameover = isMonadiusOver

newtype Monadius = Monadius (GameVariables,[GameObject])

getVariables :: Monadius -> GameVariables
getVariables (Monadius (vs,_))=vs

data GameVariables = GameVariables {
  totalScore :: Int,hiScore :: Int ,flagGameover :: Bool,
  nextTag :: Int, gameClock :: Int,baseGameLevel :: Int,playTitle :: Maybe String
  } deriving Eq

data GameObject = -- objects that are actually rendered and moved.
  VicViper{ -- player's fighter.
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hp :: Int,
    trail :: [Complex GLdouble],
    speed :: GLdouble,
    powerUpPointer :: Int,
    powerUpLevels :: Array Int Int,
    reloadTime :: Int,weaponEnergy :: Int,
    ageAfterDeath :: Int
    } |
  Option{ -- trailing support device.
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,
    optionTag :: Int,
    reloadTime :: Int,weaponEnergy :: Int} |
  StandardMissile{
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hp :: Int,mode :: Int,
    velocity :: Complex GLdouble,parentTag :: Int,probe :: GameObject } | -- missile that fly along the terrain
  Probe{ -- this lets missile to fly along the terrain
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hp :: Int
  } |
  StandardRailgun{
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hitDispLand :: Shape,hp :: Int,
    velocity :: Complex GLdouble,parentTag :: Int } | -- normal & double shot
  StandardLaser{
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hitDispLand :: Shape,hp :: Int,
    velocity :: Complex GLdouble,parentTag :: Int,age :: Int } | -- long blue straight laser
  Shield{
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hitDispLand :: Shape,hp :: Int,
    settled :: Bool,size :: GLdouble,placement :: Complex GLdouble,
    angle :: GLdouble,omega :: GLdouble} |  -- solid state of Reek power that protects enemy atacks
  PowerUpCapsule{
    tag :: Maybe Int,position :: Complex GLdouble,hitDisp :: Shape,hp :: Int,age :: Int} |
  PowerUpGauge{
    tag :: Maybe Int,position :: Complex GLdouble} |

  DiamondBomb{
    tag :: Maybe Int,position :: Complex GLdouble,velocity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int} |  -- Bacterian's most popular warhead
  TurnGear{
    tag :: Maybe Int,position :: Complex GLdouble,velocity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int,mode :: Int,
    managerTag :: Int} |  -- one of small Bacterian lifeforms, often seen in a squad.
  SquadManager{
    tag :: Maybe Int,position :: Complex GLdouble,interval :: Int,age :: Int,
    bonusScore :: Int,currentScore :: Int,
    members :: [GameObject],items :: [GameObject]
  } |
  -- 1. generates objects contained in <members> with <interval>, one at each time.
  -- 2. sticks to one of the still-alive troop members.
  -- 3. counts up <currentScore> every time when one of the squad members are destroyed by lack of hp.
  -- 4. doesn't count up <currentScore> if a squad member are destroyed by scrolling out.
  -- 5. dies when all squad members were destroyed. at this time,
  --        releases <items> if <currentScore> >= <bonusScore>, or
  --        doesn't ,if not.
  Jumper{
    tag :: Maybe Int,position :: Complex GLdouble,velocity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,gravity :: Complex GLdouble,
    touchedLand :: Bool,jumpCounter :: Int
  } | -- dangerous multi way mine dispenser.

  Grashia{
    tag :: Maybe Int,position :: Complex GLdouble,velocity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,gravity :: Complex GLdouble,
    gunVector :: Complex GLdouble,mode :: Int
  } | -- fixed antiaircraft cannon.

  Ducker{
    tag :: Maybe Int,position :: Complex GLdouble,velocity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,gVelocity :: Complex GLdouble,
    charge :: Int,vgun :: Complex GLdouble,touchedLand :: Bool
  } | -- 2-feet mobile land to air attack device.

  Flyer{
    tag :: Maybe Int,position :: Complex GLdouble,velocity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,mode :: Int
  } | -- Baterian's standard interceptor.

  ScrambleHatch{
    tag :: Maybe Int,position :: Complex GLdouble,gateAngle :: GLdouble,gravity :: Complex GLdouble,
    hitDisp :: Shape,hp :: Int,age :: Int,launchProgram :: [[GameObject]]
  } | -- Where Baterian larvae spend last process of maturation.

  LandScapeBlock{
    tag :: Maybe Int, position :: Complex GLdouble,hitDisp :: Shape,velocity :: Complex GLdouble
  } | -- landscape that just look like, and hit like its hitDisp.

  Particle{
    tag :: Maybe Int, position :: Complex GLdouble, velocity :: Complex GLdouble,
    size :: GLdouble,particleColor :: Color3 GLdouble,age :: Int,decayTime :: GLdouble,expireAge :: Int
  } | -- multi purpose particles that vanishes after expireAge.

  Star{
    tag :: Maybe Int, position :: Complex GLdouble,particleColor :: Color3 GLdouble
  } | -- background decoration
  SabbathicAgent{
    tag :: Maybe Int, fever :: Int
  } | -- generates many flyers for additional fun if there are none of them
  DebugMessage {tag :: Maybe Int,debugMessage :: String} |
  ScoreFragment{tag :: Maybe Int,score :: Int}

data HitClass = BacterianShot |
                BacterianBody |
                LaserAbsorber |
                MetalionShot |
                MetalionBody |
                ItemReceiver |
                PowerUp |
                LandScape
                deriving(Eq)

data WeaponType = NormalShot | Missile | GLdoubleShot | Laser
                  deriving (Eq)
-- WeaponType NormalShot | Missile | GLdoubleShot | Laser ... represents function of weapon that player selected, while
-- GameObject StandardRailgun | StandardLaser ... represents the object that is actually shot and rendered.
-- for example;
-- shooting NormalShot :: WeaponType and GLdoubleShot :: WeaponType both result in StandardRailgun :: GameObject creation, and
-- shooting Laser :: WeaponType creates StandardLaser :: GameObject when player is operating VicViper, or RippleLaser :: GameObject when LordBritish ... etc.

data ScrollBehavior = Enclosed{doesScroll :: Bool} | NoRollOut{doesScroll :: Bool}
 | RollOutAuto{doesScroll :: Bool, range :: GLdouble}  | RollOutFold{doesScroll :: Bool}

-----------------------------
--
--   initialization
--
------------------------------
initialMonadius :: GameVariables -> Monadius
initialMonadius initVs = Monadius (initGameVariables,initGameObjects)
    where
      initGameVariables = initVs
      initGameObjects =
          stars ++ [freshVicViper,freshPowerUpGauge]
      stars = take 26 $ map (\(t,i) -> Star{tag=Nothing,position = (fix 320 t:+fix 201 t),particleColor=colors!!i}) $ zip (map (\x -> square x + x + 41) [2346,19091..]) [1..]
      fix :: Int -> Int -> GLdouble
      fix limit value = intToGLdouble $ (value `mod` (2*limit) - limit)
      colors = [Color3 1 1 1,Color3 1 1 0,Color3 1 0 0, Color3 0 1 1] ++ colors
    --  ++ map (\x -> freshOption{optionTag = x}) [1..4]  -- full option inchiki

{-
Default settings of game objects and constants
-}
downButton,leftButton,missileButton,powerUpButton,rightButton,selfDestructButton,shotButton,upButton :: Key
downButton = SpecialKey KeyDown
leftButton = SpecialKey KeyLeft
missileButton = Char 'x'
powerUpButton = Char 'c'
rightButton = SpecialKey KeyRight
selfDestructButton = Char 'g'
shotButton = Char 'z'
upButton = SpecialKey KeyUp

-- Cuteness to add later
-- konamiCommand = [upButton,upButton,downButton,downButton,leftButton,rightButton,leftButton,rightButton,missileButton,shotButton]

gaugeOfMissile,gaugeOfGLdouble,gaugeOfLaser,gaugeOfShield :: Int
gaugeOfMissile = 1
gaugeOfGLdouble  = 2
gaugeOfLaser   = 3
gaugeOfShield  = 5

stageClearTime :: Int
stageClearTime = 7800

-- these lists are game rank modifiers.
bacterianShotSpeedList, duckerShotWay, jumperShotFactor, grashiaShotSpeedFactor :: [GLdouble]
bacterianShotSpeedList = [8,4,6,8] ++ cycle [12,8]
duckerShotCount = [2,1,1,3] ++ repeat 2
duckerShotWay = [1,1,2,1] ++ cycle [2,2]
jumperShotFactor = [0.5,0.5,0.5,0.5] ++ cycle [0.8,0.5]
grashiaShotSpeedFactor = [1,1,1,1] ++ cycle [1,0.6]

flyerHitBack, particleHitBack, powerUpCapsuleHitBack, scrambleHatchHitBack, treasure, turnGearHitBack :: [Bool]
flyerHitBack = [False,False,False] ++ repeat True
particleHitBack = True:repeat False
powerUpCapsuleHitBack = [False,False,False,False] ++ cycle [False,True]
scrambleHatchHitBack = [False,False,False,False] ++ cycle [False,True]
treasure = [False,False,False,False] ++ cycle [False,True]
turnGearHitBack = [False,False,False] ++ repeat True

duckerShotCount, flyerShotInterval, grashiaShotHalt, grashiaShotInterval, inceptorShotInterval, jumperShotWay, landRollShotInterval, scrambleHatchLaunchLimitAge :: [Int]
flyerShotInterval = [30,infinite,30,15] ++ cycle [15,15]
grashiaShotHalt = [50,100,50,50] ++ cycle [0,0]
grashiaShotInterval = [30,60,30,30] ++ cycle [15,5]
inceptorShotInterval = [45,infinite,60,45] ++ cycle [45,45]
jumperShotWay = [16,4,8,16] ++ cycle [24,32]
landRollShotInterval = [60,120,60,60] ++ cycle [30,60]
scrambleHatchLaunchLimitAge = [400,200,400,400] ++ cycle [600,400]

shotSpeed,laserSpeed,laserBreadth :: GLdouble
shotSpeed = 25
laserSpeed = 60
laserBreadth = 20

landScapeSensitive :: GameObject -> Bool
landScapeSensitive StandardRailgun{} = True -- these objects has hitDispLand
landScapeSensitive StandardLaser{} = True   -- in addition to hitDisp
landScapeSensitive Shield{} = True
landScapeSensitive _ = False

vicViperSize :: GLdouble
vicViperSize = 6

shieldPlacementMargin,shieldHitMargin :: GLdouble
shieldPlacementMargin = 5
shieldHitMargin = 10

shieldMaxHp, hatchHP :: Int
shieldMaxHp = 16
hatchHP = 15

diamondBombSize,smallBacterianSize,hatchHeight :: GLdouble
diamondBombSize = 6
hatchHeight = 35
smallBacterianSize = 16

freshDiamondBomb, freshFlyer, freshInterceptor, freshOption, freshPowerUpCapsule, freshPowerUpGauge, freshShield, freshStalk, freshStandardLaser, freshStandardMissile, freshStandardRailgun, freshTurnGear, freshTurnGearSquad, freshVicViper :: GameObject
freshDiamondBomb = DiamondBomb{tag=Nothing,position=0:+0,velocity=0:+0,hp=1,hitDisp=Circular (0:+0) diamondBombSize,age=0}
freshFlyer = Flyer{tag=Nothing,position=0:+0,velocity=(-3):+0,hitDisp=Circular 0 smallBacterianSize,hp=1,age=0,hasItem=False,mode=0}
freshInterceptor = freshFlyer{mode=1,velocity = 0:+0}
freshOption = Option{tag = Nothing, position=0:+0, hitDisp = Circular (0:+0) 0,optionTag = 0,reloadTime=0,weaponEnergy=100}
freshPowerUpCapsule = PowerUpCapsule{tag = Nothing, hitDisp = Circular (0:+0) 30,position = 0:+0,hp=1,age=0}
freshPowerUpGauge = PowerUpGauge{tag=Nothing, position = (-300):+(-240)}
freshShield = Shield{tag=Nothing,position=380:+0,hitDisp=Circular (0:+0) 0,hitDispLand=Circular (0:+0) 0,hp=shieldMaxHp,settled=False,size=0,placement=0:+0,angle=0,omega=0}
freshStalk = freshFlyer{mode=10,velocity = (-2):+0}
freshStandardLaser = StandardLaser{tag=Nothing,position=0:+0,hitDisp=Rectangular (laserSpeed/(-2):+(-laserBreadth)) (laserSpeed/2:+laserBreadth),hitDispLand = Rectangular (laserSpeed/(-2):+(-vicViperSize)) (laserSpeed/2:+vicViperSize),velocity=laserSpeed:+0,hp=1,parentTag=0,age=0}
freshStandardMissile = StandardMissile{tag=Nothing,position=0:+0,hitDisp=Circular 0 7,velocity=0:+0,hp=1,parentTag=0,probe=Probe{tag=Nothing,position=0:+0,hitDisp=Circular (0:+(-5)) 12,hp=1},mode=0}
freshStandardRailgun = StandardRailgun{tag=Nothing,position=0:+0,hitDisp=Circular 0 12,hitDispLand = Circular (0:+0) vicViperSize,velocity=shotSpeed:+0,hp=1,parentTag=0}
freshTurnGear = TurnGear{tag=Nothing,position=0:+0,velocity=0:+0,hp=1,hitDisp=Circular (0:+0) smallBacterianSize,age=0,managerTag=0,mode=0}
freshTurnGearSquad = SquadManager{tag=Nothing,position=0:+0, interval=10, age=0,
  bonusScore=squadSize, currentScore=0, members = replicate squadSize freshTurnGear,items=[freshPowerUpCapsule]} where
  squadSize=6
freshVicViper = VicViper{tag = Nothing, position = 0:+0, hitDisp = Circular (0:+0) vicViperSize,hp=1, trail = repeat $ 0:+0,
      speed = 1,powerUpPointer=(-1),powerUpLevels=array (0,5) [(x,0)|x<-[0..5]],reloadTime=0,weaponEnergy=100,
      ageAfterDeath = 0}

freshDucker :: GLdouble -> GameObject
freshDucker vg = Ducker{tag = Nothing, position = 0:+0, velocity= 0:+0, hitDisp = Circular (0:+0) smallBacterianSize, hp = 1,
  age = 0, hasItem = False, gVelocity = 0:+(8*vg),charge = 0, vgun = 0:+0,touchedLand=False}
freshScrambleHatch :: GLdouble -> GameObject
freshScrambleHatch sign = ScrambleHatch{tag=Nothing,position=0:+0,hitDisp=regulate $ Rectangular ((-45):+0) (45:+(hatchHeight*(-sign))),gravity=(0:+sign),hp=hatchHP,age=0,
  launchProgram = cycle $ replicate 40 [] ++ (concat.replicate 6) ([[freshInterceptor{velocity = 0:+(-6)*sign}]]++replicate 9 []),gateAngle=0
  }

freshVolcano :: GLdouble -> GameObject
freshVolcano grvty = LandScapeBlock{tag=Nothing,position=0:+0,velocity=0:+0,hitDisp=
  Shapes $ map (regulate.(\i -> Rectangular ((120 - 33*i + 2*i*i):+ sign*30*i) ((33*i - 2*i*i - 120) :+ sign*30*(i+1)) ) ) [0..4]}
  where sign = (-grvty)

freshTable :: GLdouble -> GameObject
freshTable grvty =  LandScapeBlock{tag=Nothing,position=0:+0,velocity=0:+0,hitDisp= Shapes $ map (regulate.(\i -> Rectangular ((-2**(i+3)+shiftSinePi i):+sign*30*i) ((2**(i+3)+shiftSinePi i) :+sign*30*(i+1)))) [0..4]
} where
    sign = (- grvty)

    shiftSinePi :: (Floating a) => a -> a
    shiftSinePi a = 5 * sin (a*0.5*pi)

freshGrashia, freshJumper, freshLandRoll :: GLdouble -> GameObject
freshGrashia sign= Grashia{tag=Nothing,position=0:+0,velocity=0:+0,
  hitDisp=Circular 0 smallBacterianSize,hp=1,age=0,hasItem=False,gravity=(0:+sign),gunVector=0:+0,mode=0}
freshJumper sign=Jumper{tag=Nothing,position=0:+0,velocity=0:+0,
                    hitDisp=Circular 0 smallBacterianSize,hp=1,age=0,hasItem=False,gravity=(0:+0.36*sign),touchedLand=False,jumpCounter=0}
freshLandRoll sign = (freshGrashia sign){mode=1}

freshLandScapeGround, freshSabbathicAgent :: GameObject
freshLandScapeGround = LandScapeBlock{tag=Nothing,position=0:+0,velocity=0:+0,hitDisp=Rectangular ((-158):+(-20)) (158:+20)}
freshSabbathicAgent = SabbathicAgent{tag=Nothing,fever=1}

freshScore :: Int -> GameObject
freshScore point = ScoreFragment{tag=Nothing,score = point}

-----------------------------
--
--  game progress
--
-----------------------------

updateMonadius :: [Key] -> Monadius -> Monadius
updateMonadius realKeys (Monadius (variables,objects))
 = Monadius (newVariables,newObjects) where
  gameVariables = variables
  gameObjects   = objects
  gameLevel = baseGameLevel gameVariables
  bacterianShotSpeed = bacterianShotSpeedList!!gameLevel


  keys = if hp vicViper<=0 then [] else realKeys
   -- almost all operation dies when vicViper dies. use realKeys to fetch unaffected keystates.

  (newNextTag,newObjects) = issueTag (nextTag variables) $
                            (loadObjects ++) $
                            filterJust.map scroll $
                            concatMap updateGameObject $
                            gameObjectsAfterCollision
  gameObjectsAfterCollision = collide objects
  -- * collision must be done BEFORE updateGameObject(moving), for
  --   players would like to see the moment of collision.
  -- * loading new objects after collision and moving is nice idea, since
  --   you then have new objects appear at exact place you wanted them to.
  -- * however, some operation would like to refer the result of the collision
  --   before it is actually taken effect in updateGameObject.
  --   such routine should use gameObjectsAfterCollision.


  newVariables = variables{
    nextTag = newNextTag,
    flagGameover = flagGameover variables ||  ageAfterDeath vicViper > 240,
    gameClock = (\c -> if hp vicViper<=0 then c else if goNextStage then 0 else c+1) $ gameClock variables,
    baseGameLevel = (\l -> if goNextStage then l+1 else l) $ baseGameLevel variables,
    totalScore = newScore,
    hiScore = max (hiScore variables) newScore
  }
   where
    goNextStage = gameClock variables > stageClearTime
    newScore = totalScore variables +  (sum) (map (\obj -> case obj of
      ScoreFragment{score = p} -> p
      _ -> 0) objects  :: [Int])

  updateGameObject :: GameObject -> [GameObject]
  -- update each of the objects and returns list of resulting objects.
  -- the list usually includes the modified object itself,
  --   may include several generated objects such as bullets and explosions,
  --   or include nothing if the object has vanished.

  updateGameObject vic@VicViper{} = newShields ++ makeMetalionShots vic{
    position=position vic + (vmag*(speed vic):+0) * (vx:+vy) ,
    trail=(if isMoving then ((position vic-(10:+0)):) else id) $ trail vic,
    powerUpLevels =
      (modifyArray gaugeOfShield (const (if (shieldCount > 0) then 1 else 0))) $
      (if doesPowerUp then (
      modifyArray (powerUpPointer vic)
        (\x -> if x<powerUpLimits!!powerUpPointer vic then x+1 else 0) . -- overpowering-up results in initial powerup level.
      (if powerUpPointer vic==gaugeOfGLdouble then modifyArray gaugeOfLaser (const 0) else id) . -- laser and double are
      (if powerUpPointer vic==gaugeOfLaser then modifyArray gaugeOfGLdouble (const 0) else id) ) --  exclusive equippment.
        else id) (powerUpLevels vic),
    powerUpPointer = if doesPowerUp then (-1) else powerUpPointer vic,
    speed = speeds !! (powerUpLevels vic!0),
    reloadTime = max 0 $ reloadTime vic - 1,
    ageAfterDeath = if hp vic>0 then 0
                    else ageAfterDeath vic+1,
    hitDisp = if treasure!!gameLevel then Circular 0 0 else Circular (0:+0) vicViperSize,
    hp = if selfDestructButton `elem` keys then 0 else hp vic
  } where
    vx = (if (rightButton `elem` keys) then 1    else 0) +
         (if (leftButton  `elem` keys) then (-1) else 0)
    vy = (if (upButton    `elem` keys) then 1    else 0) +
         (if (downButton  `elem` keys) then (-1) else 0)
    vmag = if vx*vx+vy*vy>1.1 then sqrt(0.5) else 1
    isMoving = any (\b ->  elem b keys) [rightButton,leftButton,upButton,downButton]
    doesPowerUp = (powerUpButton `elem` keys) && (powerUpPointer vic >=0) &&
      (powerUpPointer vic ==0 ||
       powerUpLevels vic!powerUpPointer vic<powerUpLimits!!powerUpPointer vic)
    speeds = [2,4,6,8,11,14] ++ speeds
    shieldCount :: Int
    shieldCount = sum $ map (\o -> case o of
      Shield{} -> 1
      _ -> 0) gameObjects
    newShields = if (doesPowerUp && powerUpPointer vic==gaugeOfShield) then
      [freshShield{position=350:+260   ,placement=40:+shieldPlacementMargin,   angle=30,omega=10   },
       freshShield{position=350:+(-260),placement=40:+(-shieldPlacementMargin),angle=0 ,omega=(-10)}]
      else []

  updateGameObject option@Option{} = makeMetalionShots option{
    position = trail vicViper !! (10*optionTag option),
    reloadTime = max 0 $ reloadTime option - 1
  }

  updateGameObject miso@StandardMissile{} = [
                                       miso{position=newpos,
                                            mode = newmode,
                                            velocity=v,
                                            probe = (probe miso){position=newpos,hp=1}
                                           }
                                      ]  where
      newmode = if hp(probe miso) <= 0 then 1 else
        if mode miso == 0 then 0 else 2
      v = case newmode of
        0 -> 3.5:+(-7)
        1 -> 8:+0
        2 -> 0:+(-8)
        _ -> 0
      newpos = position miso + v

  updateGameObject shot@StandardRailgun{} = if hp shot <=0 then [] else
    [shot{position=position shot + velocity shot}]

  updateGameObject laser@StandardLaser{} = if hp laser <=0 then [] else
    [laser{position=(\(x:+_) -> x:+parentY) $ position laser + velocity laser,age=age laser+1}]  where
      myParent = head $ filter (\o -> tag o==Just (parentTag laser)) gameObjects
      _:+parentY = position myParent

  updateGameObject shield@Shield{} = if(hp shield<=0) then [] else [
    (if settled shield then
      shield{
        position=target,size=shieldPlacementMargin+intToGLdouble (hp shield),
        hitDisp = Circular (0:+0) (size shield+shieldHitMargin),
        hitDispLand = Circular (0:+0) (size shield)
      }
    else
      shield{hp=shieldMaxHp,size=5+intToGLdouble (hp shield),position=newPosition,settled=chaseFactor>0.6})
    {angle=angle shield + omega shield}
    ] where
      newPosition = position shield + v
      v =  difference * (chaseFactor:+0)
      chaseFactor = (10/magnitude difference)
      difference = target-position shield
      target = position vicViper+(realPart (placement shield) :+ additionalPlacementY)
      additionalPlacementY = signum (imagPart$placement shield)*size shield

  updateGameObject pow@PowerUpCapsule{} = if(hp pow<=0) then [freshScore 800] else [
    pow{age=age pow + 1}
    ] ++ if powerUpCapsuleHitBack!!gameLevel && age pow ==1 then
     map (\theta -> freshDiamondBomb{position=position pow,velocity=mkPolar (bacterianShotSpeed*0.5) theta}) $
       take 8 $ iterate (+(2*pi/8)) (pi/8)  else []


  updateGameObject bullet@DiamondBomb{} = if hp bullet<=0 then [] else
    [bullet{position=position bullet + velocity bullet,age=age bullet+1}]
  updateGameObject self@TurnGear{position=pos@(x:+y),mode=m} = if hp self<=0 then
     [freshScore 50] ++ freshExplosions pos ++ if turnGearHitBack!!gameLevel then [scatteredNeraiDan pos (bacterianShotSpeed:+0)] else []
   else [
    self{
      position = position self + velocity self,
      age = age self + 1,
      mode = newmode,
      velocity = newv
    }] where
      newv = case m of
        0 -> ((-4):+0)
        1 -> if (y - (imagPart.position) vicViper) > 0 then (3:+(-5)) else (3:+(5))
        _ -> if isEasy then 2:+0 else 6:+0
      newmode = if m==0 && x < (if not isEasy then -280 else 0) && (realPart.position) vicViper> (-270) then 1 else
                    if m==1 && abs (y - (imagPart.position) vicViper) < 20 then 2 else m

  updateGameObject me@SquadManager{position=pos,interval=intv,members=membs,age=clock,tag=Just myTag} =
    if mySquadIsWipedOut then(
      if currentScore me >= bonusScore me then map (\o -> o{position=pos}) (items me) else []
     )else me{
      age = age me + 1,
      currentScore = currentScore me + todaysDeaths,
      position = if clock <= releaseTimeOfLastMember then pos else warFront
    }:dispatchedObjects where
      dispatchedObjects = if (clock `div` intv < length membs && clock `mod` intv == 0) then
        [(membs!!(clock `div` intv)){position=pos,managerTag=myTag}] else []
      todaysDeaths = sum $ map (\o -> if hp o <=0 then 1 else 0) $ mySquad
      mySquadIsWipedOut = clock > releaseTimeOfLastMember && length mySquad <= 0
      warFront = position $ head $ mySquad
      releaseTimeOfLastMember = intv * (length membs-1)
      mySquad = filter (\o -> case o of
        TurnGear{managerTag=hisManagerTag} -> hisManagerTag == myTag  -- bad, absolutely bad code
        _                                  -> False) gameObjectsAfterCollision

  updateGameObject this@Flyer{position=pos@(x:+_),age=myAge,mode=m,velocity =v} =
    if gameClock variables > stageClearTime - 100 then freshExplosions pos else
    if hp this <=0 then ([freshScore (if mode this == 10 then 30 else 110)] ++ freshExplosions pos++(if hasItem this then [freshPowerUpCapsule{position=pos}] else if flyerHitBack!!gameLevel then [scatteredNeraiDan pos (bacterianShotSpeed:+0)] else []))
     else
    [this{
      age=myAge+1,
      position = pos + v ,
      velocity = newV,
      mode = newMode
    }]++myShots where
      newV = case m of
        00 -> (realPart v):+sin(intToGLdouble myAge / 5)
        01 -> v --if magnitude v <= 0.01 then if imagPart (position vicViper-pos)>0 then 0:+10 else 0:+(-10) else v
        02 -> (-4):+0
        10 -> if (not isEasy) || myAge < 10 then stokeV else v
        _ -> v
      stokeV = angleAccuracy 16 $ (* ((min (speed vicViper*0.75) (intToGLdouble$round$magnitude v)):+0) ) $ unitVector $ position vicViper-pos
      newMode = case m of
        01 -> if myAge > 20 && (position vicViper - pos) `innerProduct` v < 0 then 02 else 01
        _  -> m
      myShots = if (myAge+13*(fromJust $tag this)) `mod` myInterval == 0 && (x <= (-80) || x <= realPart(position vicViper))
        then [jikiNeraiDan pos (bacterianShotSpeed:+0)] else []
      myInterval = if m==00 || m==03 then flyerShotInterval!!gameLevel else inceptorShotInterval!!gameLevel

  updateGameObject me@Ducker{position=pos@(_:+_),velocity = v,age=myAge,gVelocity= vgrav,touchedLand = touched} =
    if hp me <=0 then ([freshScore 130] ++ freshExplosions pos ++ if hasItem me then [freshPowerUpCapsule{position=pos}] else[]) else
    [me{
      age=myAge+1,
      position = pos + v,
      charge = if charge me <=0 && aimRate > 0.9 && aimRate < 1.1 then (duckerShotCount!!gameLevel)*7+3
        else ((\x -> if x>0 then x-1 else x)  $  charge me),
      vgun = unitVector $ aimX:+aimY,
      velocity = if charge me >0 then 0:+0 else
        if magnitude v <= 0.01 then (
          if realPart(position vicViper - pos)>0 then 3:+0 else (-3):+0
        ) else if touched then
          ((realPart v):+(-imagPart vgrav))
        else (realPart v:+(imagPart vgrav)),
      touchedLand = False
    }]++myShots where
      aimX:+aimY = position vicViper - pos
      aimRate = (-(signum$realPart v))*aimX / (abs(aimY) +0.1)
      myShots = if charge me `mod` 7 /= 6 then [] else
        map (\w -> freshDiamondBomb{position=pos,velocity=w}) vs
      vs = map (\vy -> (vgun me)*(bacterianShotSpeed:+(1.5*(vy)))) [-duckerShotWay!!gameLevel+1 , -duckerShotWay!!gameLevel+3 .. duckerShotWay!!gameLevel-0.9]

  updateGameObject me@Jumper{position=pos@(_:+_),velocity = v,age=_,gravity= g,touchedLand = touched} =
    if hp me <=0 then ([freshScore 300] ++ freshExplosions pos ++if hasItem me then [freshPowerUpCapsule{position=pos}] else[]) else
    [
      me{
        position = pos + v,
        velocity = if touched then (signum(realPart $ position vicViper-pos)*abs(realPart v):+imagPart(jumpSize*g)) else v + g,
        jumpCounter = (if touched && v`innerProduct` g >0 then (+1) else id) $
          (if doesShot then (+1) else id) $ jumpCounter me,
        touchedLand = False
      }
    ] ++shots where
    jumpSize = if jumpCounter me `mod` 4 == 2 then (-30) else (-20)
    doesShot = jumpCounter me `mod` 4 == 2 && v`innerProduct` g >0
    shots = if doesShot then
      map (\theta -> freshDiamondBomb{position=pos,velocity=mkPolar (bacterianShotSpeed*jumperShotFactor!!gameLevel) theta}) $
        take way $ iterate (+(2*pi/intToGLdouble way)) 0
      else []
    way=jumperShotWay!!gameLevel

  updateGameObject me@ScrambleHatch{position = pos,age=a} =
    if hp me <=0 then [freshScore 3000] ++ freshMiddleExplosions pos ++
      if scrambleHatchHitBack!!gameLevel then hatchHitBacks else []
     else
    [me{
      age = a + 1,
      gateAngle = max 0$ min pi$ (if length currentLaunches>0 then (+1) else (+(-0.05))) $ gateAngle me
    }] ++ currentLaunches where
      currentLaunches = if a <= scrambleHatchLaunchLimitAge!!gameLevel then
          (map (\obj -> obj{position = pos}) $ launchProgram me!!a)
        else []
      hatchHitBacks =
        (map (\theta -> freshDiamondBomb{position=pos-16*gravity me,velocity=mkPolar (bacterianShotSpeed*0.5) theta}) $ take way $ iterate (+2*pi/intToGLdouble way) 0 )++
        (map (\theta -> freshDiamondBomb{position=pos-16*gravity me,velocity=mkPolar (bacterianShotSpeed*0.4) theta}) $ take way $ iterate (+2*pi/intToGLdouble way) (pi/intToGLdouble way) )
      way = 16


  updateGameObject me@Grashia{position = pos} =
    if hp me <=0 then ([freshScore 150] ++ freshExplosions pos++ if hasItem me then [freshPowerUpCapsule{position=pos}] else[]) else
    [
      me{
        age=age me+1,
        gunVector = unitVector $ position vicViper - pos,
        position = position me + ((-3)*sin(intToGLdouble (age me*mode me)/8):+0)
      } --V no shotto wo osoku
    ] ++ if age me `mod` myInterval == 0 && age me `mod` 200 > grashiaShotHalt!!gameLevel then
      [jikiNeraiDanAc (pos+gunVector me*(16:+0)) (grashiaShotSpeedFactor!!gameLevel*bacterianShotSpeed:+0) 64] else [] where
      myInterval = if mode me == 0 then grashiaShotInterval!!gameLevel else landRollShotInterval!!gameLevel

  updateGameObject me@Particle{position = pos} =
    if age me > expireAge me then (if particleHitBack!!gameLevel then [freshScore 10,scatteredNeraiDan pos (bacterianShotSpeed:+0)] else []) else
    [me{
      age = age me + 1,
      position = position me + (decay:+0) * velocity me
    }] where
      decay =  exp $  -  intToGLdouble (age me) / decayTime me

  updateGameObject me@LandScapeBlock{position = pos,velocity = v} = [me{position = pos+v}]

  updateGameObject DebugMessage{} = []

  updateGameObject ScoreFragment{} = []

  updateGameObject me@SabbathicAgent{fever = f} = if gameClock variables>stageClearTime-180  then [] else [
    me{
      fever = if launch then f+1 else f
    }]
    ++ if launch then map (\pos -> freshStalk{position = pos,velocity=(-4):+0,hasItem = (realPart pos>0 && (round $ imagPart pos :: Int) `mod` (3*round margin)==0)}) $
    concat $ map (\t -> [(340:+t),((-340):+t),(t:+(260)),(t:+(-260))]) $[(-margin*df),(((negate margin) * df) + (margin * 2))..(margin*df+1)] else [] where
      launch = (<=0) $ length $ filter (\obj -> case obj of
        Flyer{} -> True
        _ -> False) objects
      df = intToGLdouble f - 1
      margin :: GLdouble
      margin = 20

  updateGameObject x = [x]

  makeMetalionShots :: GameObject -> [GameObject]
  {- this generates proper playerside bullets
  according to the current power up state of vicviper.
  both options and vicviper is updated using this. -}

  makeMetalionShots obj = obj{reloadTime=reloadTime obj+penalty1+penalty2,
                              weaponEnergy = max 0 $ min 100 $ weaponEnergy obj + if doesLaser then (-10) else 50 }
                                      :(shots ++ missiles) where
    (shots,penalty1) = if doesNormal then ([freshStandardRailgun{position=position obj,parentTag=myTag}] ,2)
      else if doesGLdouble then ([freshStandardRailgun{position=position obj,parentTag=myTag},freshStandardRailgun{position=position obj,parentTag=myTag,velocity=mkPolar 1 (pi/4)*velocity freshStandardRailgun}] ,2)
      else if doesLaser then ([freshStandardLaser{position=position obj+(shotSpeed/2:+0),parentTag=myTag}] ,1)
      else ([],0)
    penalty2 = if weaponEnergy obj <= 0 then 8 else 0
    missiles = if doesMissile then [freshStandardMissile{position=position obj}] else []
    doesShot = (isJust $tag obj) && (reloadTime obj <=0) && (shotButton `elem` keys)
    doesNormal = doesShot && elem NormalShot types && (shotCount<2)
    doesGLdouble = doesShot && elem GLdoubleShot types && (shotCount<1)
    doesLaser = doesShot && elem Laser types
    doesMissile = (isJust $tag obj) && elem Missile types && (missileButton `elem` keys) && (missileCount<=0)
    myTag = fromJust $ tag obj
    shotCount = length $ filter (\o -> case o of
                            StandardRailgun{} -> parentTag o==myTag
                            _             -> False) gameObjects
    missileCount = length $ filter (\o -> case o of
                                           StandardMissile{} -> True
                                           _                 -> False) gameObjects

    types = weaponTypes vicViper

  jikiNeraiDan :: Complex GLdouble -> Complex GLdouble -> GameObject
  -- an enemy bullet starting at position sourcePos and with relative velocity initVelocity.
  -- bullet goes straight to vicviper if initVelocity is a positive real number.
  jikiNeraiDanAc sourcePos initVelocity accuracy = freshDiamondBomb{
    position = sourcePos,
    velocity = (*initVelocity) $ (angleAccuracy accuracy) $ unitVector $ position vicViper - sourcePos
  }
  jikiNeraiDan sourcePos initVelocity = jikiNeraiDanAc sourcePos initVelocity 32

  scatteredNeraiDan :: Complex GLdouble -> Complex GLdouble -> GameObject
  -- a rather scattered jikiNeraiDan.
  scatteredNeraiDan sourcePos initVelocity = freshDiamondBomb{
    position = sourcePos,
    velocity = scatter $ (*initVelocity) $ (angleAccuracy 32) $ unitVector $ position vicViper - sourcePos
  } where
    scatter z = let (r,theta)=polar z in
      mkPolar r (theta+pi/8*((^(3::Int)).sin)((intToGLdouble $ gameClock variables) + magnitude sourcePos))

  freshExplosionParticle pos vel a = Particle{tag=Nothing,position=pos,velocity=vel,size=8,particleColor=Color3 1 0.5 0,age=a,decayTime=6,expireAge=20}

  freshExplosions pos = take 5 expls where
    expls :: [GameObject]
    expls = makeExp randoms
    randoms = [square $ sin(9801*sqrt t*(intToGLdouble$gameClock variables) + magnitude pos)|t<-[1..]]
    makeExp (a:b:c:xs) = (freshExplosionParticle pos (mkPolar (3*a) (2*pi*b)) (round $ -5*c)):makeExp xs
    makeExp _ = []

  freshMiddleExplosions pos = take 16 expls where
    expls :: [GameObject]
    expls = makeExp randoms 0
    randoms = [square $ sin(8086*sqrt t*(intToGLdouble$gameClock variables) + magnitude pos)|t<-[1..]]
    makeExp (a:b:xs) i = (freshExplosionParticle (pos+mkPolar 5 (pi/8*i)) (mkPolar (6+3*a) (pi/8*i)) (round $ -5*b)){size=16}:makeExp xs (i+1)
    makeExp _ _ = []

  -- issue tag so that each objcet has unique tag,
  -- and every object will continue to hold the same tag.
  issueTag :: Int -> [GameObject] -> (Int,[GameObject])
  issueTag nt [] = (nt,[])
  issueTag nt (x:xs) = (newNextTag',taggedX:taggedXs)
      where
        (nextTagForXs,taggedX) = if(isNothing $ tag x) then (nt+1,x{tag = Just nt}) else (nt,x)
        (newNextTag',taggedXs)  = issueTag nextTagForXs xs

  collide :: [GameObject] -> [GameObject]
  -- collide a list of GameObjects and return the result.
  -- it is important NOT to delete any object at the collision -- collide, show then delete
  collide = map personalCollide
    where
    -- each object has its own hitClasses and weakPoints.
    -- collision is not symmetric: A may crushed by B while B doesn't feel A.
    -- object X is hit by only objectsWhoseHitClassIsMyWeakPoint X.
    personalCollide :: GameObject -> GameObject
    personalCollide obj = foldr check obj $ objectsWhoseHitClassIsMyWeakPoint obj

    objectsWhoseHitClassIsMyWeakPoint :: GameObject -> [GameObject]
    objectsWhoseHitClassIsMyWeakPoint me =
      filter (\him -> not $ null $ (weakPoint me) `intersect` (hitClass him)) gameObjects

    hitClass :: GameObject -> [HitClass]
    hitClass VicViper{} = [MetalionBody,ItemReceiver]
    hitClass StandardMissile{} = [MetalionShot]
    hitClass StandardRailgun{} = [MetalionShot]
    hitClass StandardLaser{} = [MetalionShot]
    hitClass Shield{} = [MetalionBody]
    hitClass PowerUpCapsule{} = [PowerUp]

    hitClass DiamondBomb{} = [BacterianShot]
    hitClass TurnGear{} = [BacterianBody]
    hitClass Flyer{} = [BacterianBody]
    hitClass Ducker{} = [BacterianBody]
    hitClass Jumper{} = [BacterianBody]
    hitClass Grashia{} = [BacterianBody]
    hitClass ScrambleHatch{} = [BacterianBody,LaserAbsorber]

    hitClass LandScapeBlock{} = [LandScape]
    hitClass _ = []

    weakPoint :: GameObject -> [HitClass]
    weakPoint VicViper{} = [PowerUp,BacterianBody,BacterianShot,LandScape]
    weakPoint StandardMissile{} = [BacterianBody,LandScape]
    weakPoint Probe{} = [LandScape]
    weakPoint StandardRailgun{} = [BacterianBody,LandScape]
    weakPoint StandardLaser{} = [LaserAbsorber,LandScape]
    weakPoint Shield{} = [BacterianBody,BacterianShot,LandScape]
    weakPoint PowerUpCapsule{} = [ItemReceiver]

    weakPoint DiamondBomb{} = [MetalionBody,LandScape]
    weakPoint TurnGear{} = [MetalionBody,MetalionShot]
    weakPoint Flyer{} = [MetalionBody,MetalionShot]
    weakPoint Ducker{} = [MetalionBody,MetalionShot,LandScape]
    weakPoint Jumper{} = [MetalionBody,MetalionShot,LandScape]
    weakPoint Grashia{} = [MetalionBody,MetalionShot]
    weakPoint ScrambleHatch{} = [MetalionBody,MetalionShot]

    weakPoint _ = []

    -- after matching hitClass-weakPoint, you must check the shape of the pair of object
    -- to see if source really hits the target.
    check :: GameObject -> GameObject -> GameObject
    check source target = case (source, target) of
      (LandScapeBlock{},StandardMissile{}) -> if(hit source target) then (affect source target2) else target2 where
        target2 = target{probe = if(hit source p) then (affect source p) else p}
        p = probe target
      _                   -> if(hit source target) then (affect source target) else target

    -- if a is really hitting b, a affects b (usually, decreases hitpoint of b).
    -- note that landScapeSensitive objects have special hitDispLand other than hitDisp.
    -- this allows some weapons to go through narrow land features, and yet
    -- wipe out wider area of enemies.
    hit :: GameObject -> GameObject -> Bool
    hit a b = case (a,b) of
      (LandScapeBlock{},c) -> if landScapeSensitive c then (position a +> hitDisp a) >?< (position c +> hitDispLand c)
         else (position a +> hitDisp a) >?< (position b +> hitDisp b)
      _ -> (position a +> hitDisp a) >?< (position b +> hitDisp b)

    affect :: GameObject -> GameObject -> GameObject
    affect VicViper{} obj = case obj of
      pow@PowerUpCapsule{} -> pow{hp = hp pow-1}
      x                    -> x
    affect PowerUpCapsule{} obj = case obj of
      viper@VicViper{} -> viper{powerUpPointer = (\x -> if x >=5 then 0 else x+1)$powerUpPointer viper}
      _                -> error "Power capsule should not have been able to affect anything but the player craft."
    affect StandardMissile{} obj = obj{hp = hp obj-(hatchHP`div`2 + 2)} -- 2 missiles can destroy a hatch
    affect StandardRailgun{} obj = obj{hp = hp obj-(hatchHP`div`4 + 1)} -- 4 shots can also destroy a hatch
    affect StandardLaser{} obj = obj{hp = hp obj-1}
    affect Shield{} obj = obj{hp = hp obj-1}


    affect DiamondBomb{} obj = obj{hp = hp obj-1}
    affect TurnGear{} obj = obj{hp = hp obj-1}
    affect Flyer{} obj = obj{hp = hp obj-1}
    affect Ducker{} obj= obj{hp = hp obj-1}
    affect Jumper{} obj= obj{hp = hp obj-1}
    affect Grashia{} obj= obj{hp = hp obj-1}
    affect ScrambleHatch{} obj= obj{hp = hp obj-1}

    affect LandScapeBlock{} obj = case obj of
--      miso@StandardMissile{velocity=v} -> miso{velocity = (1:+0)*abs v}
      duck@Ducker{} -> duck{touchedLand=True}
      that@Jumper{} -> that{touchedLand=True}
      _ -> obj{hp = hp obj-1}

    affect _ t = t

  scroll :: GameObject -> Maybe GameObject
  -- make an object scroll.
  -- if the object is to vanish out of the screen, it becomes Nothing.
  scroll obj = let
      (x:+y) = position obj
      scrollBehavior :: GameObject -> ScrollBehavior
      scrollBehavior VicViper{} = Enclosed False
      scrollBehavior Option {}  = NoRollOut False
      -- We use the more verbose way of setting records here to guarantee
      -- 'range' is needed so -Wall doesn't get fooled.
      scrollBehavior StandardRailgun{} = RollOutAuto {doesScroll = True, range = shotSpeed}
      scrollBehavior StandardLaser{} = RollOutAuto True laserSpeed
      scrollBehavior PowerUpGauge{} = NoRollOut False
      scrollBehavior PowerUpCapsule{} = RollOutAuto True 40
      scrollBehavior Shield{} = NoRollOut False

      scrollBehavior DiamondBomb{} = RollOutAuto False 10
      scrollBehavior TurnGear{} = RollOutAuto False 20
      scrollBehavior SquadManager{} = NoRollOut False
      scrollBehavior ScrambleHatch{} = RollOutAuto True 60

      scrollBehavior LandScapeBlock{} = RollOutAuto True 160

      scrollBehavior Star{} = RollOutFold True

      scrollBehavior DebugMessage{} = NoRollOut False
      scrollBehavior ScoreFragment{} = NoRollOut False
      scrollBehavior SabbathicAgent{} = NoRollOut False

      scrollBehavior _          = RollOutAuto True 40

      scrollSpeed = if hp vicViper <= 0 then 0 else if gameClock variables <=6400 then 1 else 2
      rolledObj = if doesScroll $ scrollBehavior obj then obj{position=(x-scrollSpeed):+y} else obj
    in case scrollBehavior obj of
      Enclosed _ -> Just rolledObj{position = (max (-300) $ min 280 x):+(max (-230) $ min 230 y)}
      NoRollOut _ -> Just rolledObj
      RollOutAuto _ r -> if any (>r) [x-320,(-320)-x,y-240,(-240)-y] then Nothing
                        else Just rolledObj
      RollOutFold _ -> Just rolledObj{position = (if x< -320 then x+640 else x):+y} where
        (_:+_) = position rolledObj

  loadObjects :: [GameObject]
  -- a list of objects that are to newly loaded at this frame.

  loadObjects = if hp vicViper<=0 then [] else (case clock of
      -- stage layout.
      -- just like old BASIC code.
      150 -> [freshTurnGearSquad{position=340:+(180)}]
      300 -> [freshTurnGearSquad{position=340:+(-180)}]
      400 -> [freshTurnGearSquad{position=340:+(180)}]
      500 -> [freshTurnGearSquad{position=340:+(-180)}]
      633 -> map (\y -> freshStalk{position = 340:+y,hasItem=False }) [-120,120] ++ [freshStalk{position = 340:+0,hasItem=isEasy}]
      666 -> map (\y -> freshStalk{position = 340:+y,hasItem=isEasy}) [-130,130] ++ [freshStalk{position = 340:+0,hasItem=False}]
      700 -> map (\y -> freshStalk{position = 340:+y,hasItem=False }) [-140,140] ++ [freshStalk{position = 340:+0,hasItem=True}]
      733 -> map (\y -> freshStalk{position = 340:+y,hasItem=isEasy}) [-150,150] ++ [freshStalk{position = 340:+0,hasItem=False}]
      900 -> [freshTurnGearSquad{position=340:+(-180)}]
      1000 -> [freshTurnGearSquad{position=340:+(180)}]
      1050 -> map (\y -> freshStalk{position = 340:+y}) [-135,0,135]
      1250 -> map (\y -> freshFlyer{position = 340:+y}) [-150,-100]
      1300 -> map (\y -> freshFlyer{position = 340:+y,hasItem=True}) [100,150]
      1100 -> [freshTurnGearSquad{position=340:+(-180)},freshTurnGearSquad{position=340:+(180)}]
      1400 -> [(freshGrashia (-1)){position = 340:+(-185)},(freshGrashia 1){position = 340:+(185)}]
      1450 -> [(freshGrashia (-1)){position = 340:+(-185),hasItem=True},(freshGrashia 1){position = 340:+(185)}]
      1550 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
      1700 -> [(freshVolcano (-1)){position=479:+(-200)}]
      1900 -> map (\(g,x) ->  (freshDucker g){position=x:+g*100}) $ [(1,340),(-1,340)] ++ if not isEasy then [(1,-340),(-1,-340)] else []
      1940 -> [(freshLandRoll (1)){position = 340:+(185)}]
      1965 -> [(freshLandRoll (1)){position = 340:+(185)}]
      1990 -> [(freshLandRoll (1)){position = 340:+(185)}]
      2000 -> [(freshGrashia (-1)){position = 340:+(-185)},(freshDucker (-1)){position=(-340):+(-185)}]
      2033 -> [(freshGrashia (-1)){position = 340:+(-185)},(freshDucker 1){position=(-340):+(185)}]
      2100 -> [(freshScrambleHatch (-1)){position = 360:+(-200)}]
      2200 -> [(freshVolcano 1){position=479:+(200)}]
      2250 -> map (\y -> freshStalk{position = 340:+y}) [-150,0,150]
      2339 -> [(freshGrashia (1)){position = 340:+35},(freshGrashia (-1)){position = 340:+(-185)}]

      2433 -> map (\y -> freshFlyer{position = 340:+y}) [-150,0]
      2466 -> map (\y -> freshFlyer{position = 340:+y}) [-150,0]
      2499 -> map (\y -> freshFlyer{position = 340:+y}) [-150,0]

      2620 -> [(freshDucker 1){position=(-340):+(200)}]
      2640 -> [(freshDucker 1){position=(-340):+(200),hasItem=True}]
      2800 -> map (\(g,x) ->  (freshJumper g){position=x:+g*100,velocity=((-3)*signum x):+0}) $ [(1,340),(-1,340)] ++ if not isEasy then [(1,-340),(-1,-340)] else []
      2999 -> [(freshVolcano 2){position=479:+(20),velocity=(0:+(-0.5))},(freshVolcano (-2)){position=479:+(-20),velocity=(0:+(0.5))}]

      3200 -> concat $ map (\x -> [freshLandScapeGround{position=(479-x):+220},freshLandScapeGround{position=(479-x):+(-220)}]) [320,640]

      3210 -> [freshFlyer{position = 340:+150},freshFlyer{position = 340:+100,hasItem=True}]
      3290 -> [freshFlyer{position = 340:+(-150)},freshFlyer{position = 340:+(-100),hasItem=True}]
      3350 -> map (\g -> (freshLandRoll (g)){position = 340:+(g*185)}) [1,-1] ++ if isRevival then [] else [(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
      3400 -> map (\g -> (freshLandRoll (g)){position = 340:+(g*185)}) [1,-1] ++ if isRevival then [] else [(freshJumper (-1)){position = (-340):+(-150),velocity=3:+0}]
      3450 -> map (\g -> (freshLandRoll (g)){position = 340:+(g*185)}) [1,-1]
      3500 -> [(freshVolcano (-1)){position=479:+(-200)}]
      3579 -> [(freshGrashia (-1)){position = 340:+(-100)}]
      3639 -> [(freshGrashia (-1)){position = 340:+(-40)}]
      3699 -> [(freshGrashia (-1)){position = 340:+(-100)}]
      3501 -> [(freshScrambleHatch (1)){position = 360:+(200)}]
      3600 -> [(freshScrambleHatch (1)){position = 360:+(200)}]
      3582 -> [(freshDucker (-1)){position=(340):+(-200)}]
      3612 -> [(freshDucker (-1)){position=(340):+(-200)}]
      3642 -> [(freshDucker (-1)){position=(340):+(-200)}]
      3672 -> [(freshDucker (-1)){position=(340):+(-200)}]
      3702 -> [(freshDucker (-1)){position=(340):+(-200),hasItem=True}]
      3703 -> [(freshLandRoll (1)){position=(340):+(185),hasItem=True}]
      3820 -> map (\y -> freshFlyer{position = 340:+y}) [-100,100]
      3840 -> map (\y -> freshFlyer{position = 340:+y}) [-110,110]
      3860 -> map (\y -> freshFlyer{position = 340:+y}) [-120,120]
      3880 -> map (\y -> freshFlyer{position = 340:+y,hasItem = isEasy}) [-130,130]
      3900 -> [freshTurnGearSquad{position=340:+0}]
      4000 -> [(freshTable 1){position=450:+200}]
      4033 -> [(freshGrashia (1)){position = 340:+(185)}]
      4066 -> [(freshGrashia (1)){position = 340:+(185)}]
      4060 -> [(freshGrashia (1)){position = 340:+(40)}]
      4110 -> [(freshGrashia (1)){position = 340:+(40)}]
      4160 -> [(freshGrashia (1)){position = 340:+(40)}]
      4166 -> [(freshGrashia (1)){position = 340:+(185),hasItem=True}]
      4200 -> [(freshGrashia (1)){position = 340:+(185),hasItem=True}]
      4233 -> [(freshGrashia (1)){position = 340:+(185),hasItem=False}]
      4266 -> [(freshGrashia (1)){position = 340:+(185),hasItem=False}]
      4150 -> [freshLandScapeGround{position=479:+(-180)}]
      4203 -> [(freshJumper (-1)){position = 340:+(-180)}]
      4273 -> [(freshJumper (-1)){position = 340:+(-180)}]
      4343 -> [(freshJumper (-1)){position = 340:+(-180)}]
      4490 -> [(freshTable (-1)){position=450:+(-200)}]
      4500 -> [(freshDucker (-1)){position=340:+0}]
      4520 -> [(freshDucker (-1)){position=340:+0}]
      4540 -> [(freshDucker (-1)){position=340:+0}]
      4560 -> [(freshGrashia (-1)){position = 340:+(-185)}]
      4580 -> [(freshScrambleHatch (-1)){position = 360:+(-50)}]
      4603 -> if isRevival then [] else [(freshDucker 1){position = (-340):+0},(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
      4663 -> [(freshDucker 1){position = (-340):+0}]++if isEasy then [] else [(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
      4723 -> if isRevival then [] else [(freshDucker 1){position = (-340):+0},(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
      4783 -> [(freshDucker 1){position = (-340):+0}]++if isEasy then [] else [(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
      4680 -> [(freshScrambleHatch (-1)){position = 360:+(-200)}]
      4900 -> map (\y -> freshFlyer{position = 340:+y}) [-100,100]
      4930 -> map (\y -> freshFlyer{position = 340:+y}) [-66,66]
      4960 -> map (\y -> freshFlyer{position = 340:+y}) [-33,33]
      4990 -> map (\y -> freshFlyer{position = 340:+y,hasItem=True}) [0]
      5041 -> [(freshDucker (-1)){position = 340:+(-180)}]
      5061 -> [(freshDucker (-1)){position = 340:+(-180)}]
      5081 -> [(freshDucker (-1)){position = 340:+(-180)}]
      5101 -> if isRevival then [] else ([(freshDucker (-1)){position = 340:+(-180)}] ++ if isEasy then [] else [(freshDucker (1)){position = (-340):+(180)}])
      5121 -> if isRevival then [] else ([(freshDucker (-1)){position = 340:+(-180)}] ++ if isEasy then [] else [(freshDucker (1)){position = (-340):+(180)}])
      5141 -> if isRevival then [] else ([(freshDucker (-1)){position = 340:+(-180)}] ++ if isEasy then [] else [(freshDucker (1)){position = (-340):+(180)}])
      5261 -> [freshTurnGearSquad{position=340:+(-150)}]
      5364 -> [(freshScrambleHatch (-1)){position = 360:+(-200)}]
      5151 -> [(freshDucker (-1)){position = (-340):+(0)}]
      5181 -> [(freshDucker (-1)){position = (-340):+(0)}]
      5211 -> [(freshDucker (-1)){position = (-340):+(0)}]
      5241 -> [(freshDucker (-1)){position = (-340):+(0)}]
      5321 -> [(freshDucker (-1)){position = 340:+150}] ++ if isEasy then [] else [(freshJumper (1)){position = (-340):+(180),velocity = 3:+0}]
      5361 -> [(freshDucker (-1)){position = 340:+150}]
      5401 -> [(freshDucker (-1)){position = 340:+150}]
      5441 -> [(freshDucker (-1)){position = 340:+150}] ++ if isEasy then [] else map (\y -> freshStalk{position = 340:+y}) [-140,-70,0]
      5461 -> [(freshDucker (-1)){position = 340:+150}]
      5451 -> [(freshGrashia (-1)){position = 340:+160,hasItem=True}]

      5060 -> [(freshVolcano (-1)){position = 480:+(-100)}]
      5200 -> [(freshGrashia (-1)){position = 340:+70}] ++ [(freshDucker (-1)){position = 340:+70}]
      5235 -> [(freshGrashia (-1)){position = 340:+40}] ++ [(freshDucker (-1)){position = 340:+40}]
      5258 -> [(freshGrashia (-1)){position = 340:+10}] ++ [(freshDucker (-1)){position = 340:+10}]
      5285 -> [(freshGrashia (-1)){position = 340:+(-20)}] ++ [(freshDucker (-1)){position = 340:+(-20)}]
      5316 -> [(freshGrashia (-1)){position = 340:+(-50)}] ++ [(freshDucker (-1)){position = 340:+(-50)}]

      5310 -> [(freshVolcano (1)){position = 480:+(150)}]
      5450 -> [(freshGrashia (1)){position = 340:+(-20)}]
      5485 -> [(freshGrashia (1)){position = 340:+10}]
      5508 -> [(freshGrashia (1)){position = 340:+40}]
      5535 -> [(freshGrashia (1)){position = 340:+70}]
      5566 -> [(freshGrashia (1)){position = 340:+100}]

      5811 -> [(freshDucker (-1)){position = (-340):+0}]
      5841 -> [(freshDucker (-1)){position = (-340):+0}]
      5871 -> [(freshDucker (-1)){position = (-340):+0}]
      5901 -> [(freshDucker (-1)){position = (-340):+0}]
      6001 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []
      6031 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []
      6061 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []
      6091 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []

      5800 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
      5950 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
      6100 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
      6116 -> [freshSabbathicAgent]

      _ -> []) ++
    (if(optionCount < powerUpLevels vicViper!4) then
      [freshOption{position=position vicViper, optionTag = optionCount+1}]
      else []) ++
    (if (clock `mod` 320 == 0 && clock>=1280 && clock <= 6400) then
      [freshLandScapeGround{position=479:+220},freshLandScapeGround{position=479:+(-220)}]
      else [])

    where
      clock = gameClock variables
      optionCount = length $ filter (\o -> case o of
        Option{} -> True
        _  -> False) gameObjects
      isRevival = optionCount <= 0
  isEasy = gameLevel <= 1

  vicViper = fromJust $ find (\obj -> case obj of
                            VicViper{} -> True
                            _          -> False) objects

-- things needed both for progress and rendering
powerUpLimits :: [Int]
powerUpLimits = [5,1,1,1,4,1]
weaponTypes :: GameObject -> [WeaponType]
weaponTypes viper@VicViper{} =
  [if powerUpLevels viper!gaugeOfGLdouble>0 then GLdoubleShot else
   if powerUpLevels viper!gaugeOfLaser>0 then Laser else
   NormalShot] ++
   if powerUpLevels viper!gaugeOfMissile>0 then [Missile] else []

weaponTypes _ = []

-------------------------
--
--  drawing
--
-------------------------
renderMonadius :: Monadius -> IO ()
renderMonadius (Monadius (variables,objects)) = do
  putDebugStrLn $ show $ length objects
  mapM_ renderGameObject objects
  preservingMatrix $ do
    translate (Vector3 (-300) (220) (0 :: GLdouble))
    renderWithShade (Color3 1 1 (1 :: GLdouble)) (Color3 0 0 (1 :: GLdouble)) $ do
      scale (0.2 :: GLdouble) 0.2 0.2
      renderString MonoRoman scoreStr
  preservingMatrix $ do
    translate (Vector3 (0) (220) (0 :: GLdouble))
    renderWithShade (Color3 1 1 (1 :: GLdouble)) (Color3 0 0 (1 :: GLdouble)) $ do
      scale (0.2 :: GLdouble) 0.2 0.2
      renderString MonoRoman scoreStr2
  where
  scoreStr = "1P "  ++ ((padding '0' 8).show.totalScore) variables
  scoreStr2 = if isNothing $ playTitle variables then "HI "++((padding '0' 8).show.hiScore) variables else (fromJust $ playTitle variables)

  gameclock = gameClock variables

  -- returns an IO monad that can render the object.
  renderGameObject :: GameObject -> IO ()
  renderGameObject gauge@PowerUpGauge{} = preservingMatrix $ do
    let x:+y = position gauge
    translate (Vector3 x y 0)
    color (Color3 (1.0 :: GLdouble) 1.0 1.0)
    mapM_ (\(i,j) -> (if(i==activeGauge)then renderActive else renderNormal) j (isLimit i) i) $
      zip [0..5] [0,90..450] where
      w=80
      h=20
      renderNormal x l i = preservingMatrix $ do
        color (Color3 0.7 0.8 (0.8 :: GLdouble))
        preservingMatrix $ do
          translate (Vector3 x 0 (0 :: GLdouble))
          renderPrimitive LineLoop $ ugoVertices2D 0 1 [(0,0),(w,0),(w,h),(0,h)]
          if l then renderPrimitive Lines $ ugoVertices2D 0 1 [(0,0),(w,h),(w,0),(0,h)] else return()
        preservingMatrix $ do
          ugoTranslate x 0 0 3
          translate (Vector3 (w/2) 0 (0 :: GLdouble))
          rotate (3 * sin(intToGLdouble gameclock/10)) (Vector3 0 0 (1 :: GLdouble))
          translate (Vector3 (-w/2) 0 (0 :: GLdouble))
          renderPowerUpName i

      renderActive x l i = preservingMatrix $ do
        color (Color3 1 1 (0 :: GLdouble))
        preservingMatrix $ do
          translate (Vector3 x 0 0)
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 5 2 [(0,0),(w,0),(w,h),(0,h)]
          if l then renderPrimitive Lines $ ugoVertices2DFreq 0 5 2 [(0,0),(w,h),(w,0),(0,h)] else return()
        preservingMatrix $ do
          ugoTranslateFreq x 0 0 5 2
          translate (Vector3 (w/2) 0 (0 :: GLdouble))
          rotate (10 * sin(intToGLdouble gameclock/5)) (Vector3 0 0 (1 :: GLdouble))
          scale 1.2 1.2 (0 :: GLdouble)
          translate (Vector3 (-w/2) 0 (0 :: GLdouble))
          renderPowerUpName i
      activeGauge = powerUpPointer vicViper
      isLimit i = powerUpLevels vicViper!i>=powerUpLimits!!i
      renderPowerUpName i = do
        translate (Vector3 6 3.5 (0 :: GLdouble))
        scale (0.15 :: GLdouble) 0.13 0.15
        renderString Roman $ ["SPEED","MISSILE","DOUBLE","LASER","OPTION","  ?"]!!i

  renderGameObject vic@VicViper{position = x:+y} = if hp vic<=0 then preservingMatrix $ do
      translate (Vector3 x y 0)
      scale pishaMagnitudeX pishaMagnitudeY 0
      renderWithShade (Color3 (1.0 :: GLdouble) 0 0) (Color3 (1.0 :: GLdouble) 0.6 0.4) $ do
        renderPrimitive LineLoop $ ugoVertices2DFreq 0 1 1
          [(0,12),(8,8),(10,4),(20,0),(10,-4),(8,-8),(0,-12),(-8,-8),(-10,-4),(-20,0),(-10,4),(-8,8)]
    else preservingMatrix $ do
      translate (Vector3 x y 0)
      renderWithShade (Color3 (1.0 :: GLdouble) 1.0 1.0) (Color3 (0.4 :: GLdouble) 0.4 0.6) $ do
        renderPrimitive LineStrip $ ugoVertices2D 0 2
          [((-14),(-1)),((-12),5),((-20),13),(-14,13),(2,5),(8,1),(32,1),(32,(-1)),(24,(-3)),(16,(-3))]
        renderPrimitive LineStrip $ ugoVertices2D 0 2
          [((-10),(-1)),(14,(-1)),(18,(-5)),(4,(-9)),((-2),(-9))]
        renderPrimitive LineLoop $ ugoVertices2D 0 2
          [((-18),3),((-16),3),((-16),(-3)),((-18),(-3))]
      renderWithShade (Color3 (0.92 :: GLdouble) 0.79 0.62) (Color3 (0.75 :: GLdouble) 0.38 0.19) $ do
        renderPrimitive LineStrip $ ugoVertices2D 0 2
          [(4,3),(6,5),(14,5),(22,1)] --cockpit
      renderWithShade (Color3 (0.6 :: GLdouble) 0.8 1.0) (Color3 0.19 0.38 (0.75 :: GLdouble)) $ do
        renderPrimitive LineLoop $ ugoVertices2D 0 2
          [((-14),(-1)),((-10),(-1)),((-2),(-9)),((-4),(-9)),((-10),(-7)),((-14),(-3))] -- identification blue coting
      renderWithShade (Color3 (0 :: GLdouble) 0 0.8) (Color3 (0.0 :: GLdouble) 0.0 0.4) $ do
        renderPrimitive LineLoop $ ugoVertices2D 0 4
          [((-36),1),((-28),5),((-24),5),((-20),1),((-20),(-1)),((-24),(-5)),((-28),(-5)),((-36),(-1))] -- backfire
    where
      pishaMagnitudeX :: GLdouble
      pishaMagnitudeY :: GLdouble
      pishaMagnitudeX = max 0 $ (8*) $ (\z -> z*(1-z)) $ (/20) $ intToGLdouble $ ageAfterDeath vic
      pishaMagnitudeY = max 0 $ (5*) $ (\z -> z*(1-z)) $ (/15) $ intToGLdouble $ ageAfterDeath vic

  renderGameObject Option{position = x:+y} = preservingMatrix $ do
    translate (Vector3 x y 0)
    renderWithShade (Color3 (0.8 :: GLdouble) 0 0) (Color3 (0.4 :: GLdouble) 0 0) $
      renderPrimitive LineLoop $ ugoVertices2D 0 2
        [(5,9),(9,7),(13,3),(13,(-3)),(9,(-7)),(5,(-9)),
         ((-5),(-9)),((-9),(-7)),((-13),(-3)),((-13),3),((-9),7),((-5),9)]
    renderWithShade (Color3 (1.0 :: GLdouble) 0.45 0) (Color3 (0.4 :: GLdouble) 0.2 0) $
      renderPrimitive LineStrip $ ugoVertices2D 0 1
        [((-12.0),(3.4)),(0.8,8.7),((-8.1),(-0.9)),(4.0,5.8),(4.3,5.6),
          ((-4.4),(-6.8)),((-4.1),(-6.9)),(8.3,0.8),(9.0,0.6),(2.0,(-7.2))]

  renderGameObject StandardMissile{position=x:+y,velocity=v} = preservingMatrix $ do
    let dir = (phase v) :: GLdouble
    translate (Vector3 x y 0)
    rotate (dir / pi * 180) (Vector3 0 0 (1 :: GLdouble))
    color (Color3 (1.0 :: GLdouble) 0.9 0.5)
    renderPrimitive LineLoop $ ugoVertices2D 0 1 [(0,0),(-7,2),(-7,-2)]
    renderPrimitive LineStrip $ ugoVertexFreq (-11) 0 0 1 1 >> ugoVertexFreq (-17) 0 0 7 1

  renderGameObject StandardRailgun{position=x:+y,velocity=v} =
    preservingMatrix $ do
      let (_,phse)=polar v
      translate (Vector3 x y 0)
      rotate (phse / pi * 180) (Vector3 0 0 (1 :: GLdouble))
      color (Color3 (1.0 :: GLdouble) 0.9 0.5)
      renderPrimitive Lines $ ugoVertices2D 0 1 [(0,0),((-5),0),((-9),0),((-11),0)]

  renderGameObject laser@StandardLaser{position=x:+y,velocity=v} =
    if age laser < 1 then return ()
    else preservingMatrix $ do
      let (_,phs)=polar v
      translate (Vector3 x y 0)
      rotate (phs / pi * 180) (Vector3 0 0 (1 :: GLdouble))
      color (Color3 (0.7 :: GLdouble) 0.9 1.0)
      renderPrimitive Lines $ ugoVertices2D 0 0 [(12,0),(-laserSpeed,0)]

  renderGameObject Shield{position=x:+y, size = r,angle = theta} = preservingMatrix $ do
    translate (Vector3 x y 0)
    rotate theta (Vector3 0 0 (1 :: GLdouble))
    renderWithShade (Color3 (0.375 :: GLdouble) 0.75 0.9375) (Color3 (0.86 :: GLdouble) 0.86 0.86) $ do
      scale r r 0
      renderTriangle
      rotate 60 (Vector3 0 0 (1 :: GLdouble))
      renderTriangle where
        renderTriangle = do
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 0.1 1 $ map (\t -> (cos t,sin t)) [0,pi*2/3,pi*4/3]

  renderGameObject powerUpCapsule@PowerUpCapsule{} = preservingMatrix $ do
    let x:+y = position powerUpCapsule
    translate (Vector3 x y 0)
    renderWithShade (Color3 (0.9 :: GLdouble) 0.9 0.9) (Color3 (0.4 :: GLdouble) 0.4 0.4) $ do
      futa >> neji >> toge
      rotate (180) (Vector3 1 0 (0 :: GLdouble)) >> toge
      rotate (180) (Vector3 0 1 (0 :: GLdouble)) >> futa >> neji >> toge
      rotate (180) (Vector3 1 0 (0 :: GLdouble)) >> toge
    renderWithShade (Color3 (1.0 :: GLdouble) 0.0 0.0) (Color3 (0.3 :: GLdouble) 0.3 0.0) $ do
      nakami
      where
        futa = renderPrimitive LineStrip $ ugoVertices2D 0 1 [((-10),6),((-6),10),(6,10),(10,6)]
        neji = (renderPrimitive LineStrip $ ugoVertices2D 0 1 [(12,4),(12,(-4))]) >>
               (renderPrimitive LineStrip $ ugoVertices2D 0 1 [(16,2),(16,(-2))])
        toge = renderPrimitive LineStrip $ ugoVertices2D 0 1 [(10,8),(16,14)]
        nakami = rotate 145 (Vector3 0.2 0.2 (1 :: GLdouble)) >> scale 9 6 (1 :: GLdouble) >>
          (renderPrimitive LineStrip $ ugoVertices2D 0 0.2 $ map (\n ->  (cos$n*pi/8,sin$n*pi/8)) [1,15,3,13,5,11,7,9])

  renderGameObject DiamondBomb{position = (x:+y),age=clock} = preservingMatrix $ do
    translate (Vector3 x y 0)
    rotate (90*intToGLdouble(clock`mod`4)) (Vector3 0 0 (1 :: GLdouble))
    color (Color3 (1 :: GLdouble) 1 1)
    renderPrimitive LineLoop $ vertices2D 0 $ [a,b,c]
    color (Color3 (0.5 :: GLdouble) 0.5 0.5)
    renderPrimitive Lines $ vertices2D 0 $ [a,d,a,e]
    renderPrimitive LineStrip $ vertices2D 0 $ [c,d,e,b]
    where
      [a,b,c,d,e] = [(0,0),(r,0),(0,r),(-r,0),(0,-r)]
      r = diamondBombSize
      --    c
      --   /|\
      --  d-a-b
      --   \|/
      --    e
  renderGameObject TurnGear{position=x:+y,age=clock} = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (Color3 1.0 0.7 1.0 :: Color3 GLdouble)
    rotate (5 * intToGLdouble clock) (Vector3 0 0 1 :: Vector3 GLdouble)
    renderWing
    rotate 120 (Vector3 0 0 1 :: Vector3 GLdouble)
    renderWing
    rotate 120 (Vector3 0 0 1 :: Vector3 GLdouble)
    renderWing
    where
      renderWing = renderPrimitive LineLoop $ ugoVertices2D 0 2 $ map ((\(t:+u) -> (t,u)) . (\(r,t) -> mkPolar r (pi*t)) )
        [(3,0), (3,2/3), (smallBacterianSize,1/3), (smallBacterianSize,0), (smallBacterianSize+3,-1/3)]

  renderGameObject Flyer{position=x:+y,age=_,velocity = v,hasItem=item}  = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 GLdouble) else (Color3 0.3 1.0 0.7 :: Color3 GLdouble))
    rotate (phase v / pi * 180) (Vector3 0 0 (1 :: GLdouble))
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ [(-2,0),(-6,4),(-10,0),(-6,-4)]
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ [(2,4),(16,4),(4,16),(-10,16)]
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ [(2,-4),(16,-4),(4,-16),(-10,-16)]

  renderGameObject Ducker{position = (x:+y),hitDisp=hd,hasItem=item,velocity = v,gVelocity = g,age = a} = preservingMatrix $ do
    translate (Vector3 x y 0)
    if signum (imagPart g) > 0 then scale 1 (-1) (1 :: GLdouble) else return ()
    if signum (realPart v) < 0 then scale (-1) 1 (1 :: GLdouble) else return ()
    --after this, ducker is on the lower ground, looking right
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 GLdouble) else (Color3 0.3 1.0 0.7 :: Color3 GLdouble))
    renderShape (0:+0) hd
    renderPrimitive LineStrip $ vertices2D 0 [(0,0),(kx,ky),(fx,fy)]
    where
      fx:+fy=foot $ intToGLdouble a/2
      kx:+ky=knee $ intToGLdouble a/2
      foot theta = (16*cos(-theta)):+(-16+8*sin(-theta))
      knee theta = foot theta * (0.5 :+ (- sqrt(square(legLen/magnitude(foot theta)) - 0.25)))
      legLen = 16

  renderGameObject Jumper{position = (x:+y),hitDisp=hd,hasItem=item,gravity = g,velocity=v} = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 GLdouble) else (Color3 0.3 1.0 0.7 :: Color3 GLdouble))
    renderShape (0:+0) hd
    if gsign >0 then rotate 180 (Vector3 (1 :: GLdouble) 0 0) else return() -- after this you can assume that the object is not upside down
    renderPrimitive LineStrip $ ugoVertices2D 0 2 $ [(15,-5),(25,-5+absvy*leg),(25,-25+absvy*leg)]
    renderPrimitive LineStrip $ ugoVertices2D 0 2 $ [(-15,-5),(-25,-5+absvy*leg),(-25,-25+absvy*leg)]
    where
      gsign = signum $ imagPart g
      absvy = imagPart v * gsign -- if falling (+) ascending (-)
      leg = 1.5

  renderGameObject Grashia{position = (x:+y),hitDisp=hd,hasItem=item,gunVector = nv,gravity = g,mode=m} = preservingMatrix $ do
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 GLdouble) else (Color3 0.3 1.0 0.7 :: Color3 GLdouble))
    translate (Vector3 x y 0)
    renderShape (0:+0) hd
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ map (\r -> (nvx*r,nvy*r)) [16,32]
    if m == 1 then do
      renderShape 0 $ Circular (16:+12*gsign) 4
      renderShape 0 $ Circular ((-16):+12*gsign) 4
     else return ()
    where
      nvx:+nvy = nv
      gsign = signum $ imagPart g

  renderGameObject me@ScrambleHatch{position = (x:+y),hitDisp=_,gravity= g,gateAngle = angl} = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (Color3 (1.2*(1-hpRate)) 0.5 (1.6*hpRate)  :: Color3 GLdouble)
    if gsign >0 then rotate 180 (Vector3 (1 :: GLdouble) 0 0) else return() -- after this you can assume that the object is not upside down
    renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*2) 1 $ [(-45,1),(-45,hatchHeight),(45,hatchHeight),(45,1)]
    preservingMatrix $ do
      translate (Vector3 45 hatchHeight (0 :: GLdouble))
      rotate (-angl/pi*180) (Vector3 0 0 (1 :: GLdouble))
      renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*1) 2 $ [(0,0),(-45,0),(-45,10)]
    preservingMatrix $ do
      translate (Vector3 (-45) hatchHeight (0 :: GLdouble))
      rotate (angl/pi*180) (Vector3 0 0 (1 :: GLdouble))
      renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*1) 2 $ [(0,0),(45,0),(45,10)]
    where
      gsign = signum $ imagPart g
      hpRate = (intToGLdouble $ hp me)/(intToGLdouble hatchHP)

  renderGameObject LandScapeBlock{position=pos,hitDisp=hd} = preservingMatrix $ do
    color (Color3 0.6 0.2 0 :: Color3 GLdouble)
    renderShape pos hd
    if treasure!!(baseGameLevel variables) then do
      color (Color3 0.7 0.23 0 :: Color3 GLdouble)
      translate (Vector3 0 0 (60 :: GLdouble))
      renderShape pos hd
      color (Color3 0.5 0.17 0 :: Color3 GLdouble)
      translate (Vector3 0 0 (-120 :: GLdouble))
      renderShape pos hd
     else return()

  renderGameObject me@Particle{position = x:+y,particleColor=Color3 mr mg mb} = preservingMatrix $ do
    if age me>=0 then do
      translate (Vector3 x y 0)
      color (Color3 r g b)
      renderShape (0:+0) $ Circular (0:+0) (size me*extent)
      else return ()
    where
      extent = 0.5 +  intCut (intToGLdouble(age me) / decayTime me)
      decay =  exp $  intCut $ -intToGLdouble (age me) / decayTime me
      whiteout = exp $ intCut $  -2*intToGLdouble (age me) / decayTime me
      r = mr * decay + whiteout
      g = mg * decay + whiteout
      b = mb * decay + whiteout
      intCut :: GLdouble -> GLdouble
      intCut = intToGLdouble.round

  renderGameObject Star{position = x:+y,particleColor=c} = preservingMatrix $ do
    color c
    renderPrimitive LineStrip $ ugoVertices2D 0 2 [(0.1+x,0+y),(-0.1+x,0+y)]


  renderGameObject DebugMessage{debugMessage=str} =
    putDebugStrLn str

  renderGameObject _ = return ()
  vicViper = fromJust $ find (\obj -> case obj of
                            VicViper{} -> True
                            _          -> False) objects

  renderShape :: Complex GLdouble -> Shape -> IO ()
  renderShape (x:+y) s = case s of
    Rectangular{bottomLeft = (l:+b), topRight = (r:+t)}  ->
      renderPrimitive LineLoop $ vertices2D 0 [(x+l,y+b),(x+l,y+t),(x+r,y+t),(x+r,y+b)]
    Circular{center=cx:+cy, radius = r} -> preservingMatrix $ do
      translate (Vector3 (cx+x) (cy+y) 0)
      rotate (intToGLdouble gameclock*(45+pi)) (Vector3 0 0 (1 :: GLdouble))
      scale r r 1
      renderPrimitive LineLoop $ vertices2D 0 $ map (\t -> (cos(2/7*t*pi),sin(2/7*t*pi))) [0..6]
    Shapes{children=cs} -> mapM_ (renderShape (x:+y)) cs


  renderWithShade :: ColorComponent a=>Color3 a -> Color3 a -> IO () -> IO ()
  renderWithShade colorA colorB rndrr = do
    color colorB
    preservingMatrix $ do
      translate $ Vector3 1 (-1) (-1 :: GLdouble)
      rndrr
    color colorA
    preservingMatrix rndrr

--   ugoVertex :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
--   ugoVertex x y z r = ugoVertexFreq x y z r standardUgoInterval

  ugoVertexFreq :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> Int -> IO ()
  -- renders a vertex at somewhere near (x y z),
  -- but the point wiggles around in ugoRange when each interval comes.
  ugoVertexFreq x y z ugoRange intrvl = vertex $ Vertex3 (x+dr*cos theta) (y+dr*sin theta) z where
    flipper :: GLdouble
    flipper = fromIntegral $ (gameclock `div` intrvl) `mod` 1024
    dr = ugoRange * vibrator(phi)
    theta = (x + sqrt(2)*y + sqrt(3)*z + 573) * 400 * flipper
    phi   = (x + sqrt(3)*y + sqrt(7)*z + 106) * 150 * flipper
    vibrator a = 0.5 * (1 + sin a)

  ugoTranslate x y z ugoRange = ugoTranslateFreq x y z ugoRange standardUgoInterval
  ugoTranslateFreq x y z ugoRange intvl = translate (Vector3 (x+dr*cos theta) (y+dr*sin theta) z) where
    flipper :: GLdouble
    flipper = fromIntegral $ (gameclock `div` intvl) `mod` 1024
    dr = ugoRange * vibrator(phi)
    theta = (x + sqrt(2)*y + sqrt(3)*z + 573) * 400 * flipper
    phi   = (x + sqrt(3)*y + sqrt(7)*z + 106) * 150 * flipper
    vibrator a = 0.5 * (1 + sin a)

  ugoVertices2D z r xys = ugoVertices2DFreq z r standardUgoInterval xys
  ugoVertices2DFreq z r intrvl xys = mapM_ (\(x,y) -> ugoVertexFreq x y z r intrvl) xys

  vertices2D :: GLdouble -> [(GLdouble,GLdouble)] -> IO ()
  vertices2D z xys = mapM_ (\(x,y) -> vertex $ Vertex3 x y z) xys

standardUgoInterval :: Int
standardUgoInterval = 7

isMonadiusOver :: Monadius -> Bool
isMonadiusOver (Monadius (vars,_)) = flagGameover vars
