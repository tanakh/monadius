      Monadius for Windows with OpenGL

            presented by Hideyuki Tanaka & Takayuki Muranushi

  ... is a functional power-up horizontal scrolling shooter,
  dedicated to:
  the 20th anniversary of Gradius series,
  those who love games, and
  those who love game programming.

** how to install
   Download and unpack.
   Build and install through Cabal as for other Haskell packages:

        runhaskell Setup configure --user --prefix=$HOME
        runhaskell Setup build
        runhaskell Setup install --user

  (You may want to remove the --user flag when installing as root.)

  To play Monadius, you need OpenGL and the Haskell OpenGL bindings
  installed into your computer.

** controls
  move arrow keys
  shoot z
  missile x
  power up c
  start space
  bail out g
  quit q,escape

** how to play
  operate your ship, shoot and destroy them all.
  avoid bullets, enemies, walls, the landscape - or you'll be crushed.

** power up
  some enemies are carrying power up capsules.
  collect them and press power up button to improve your ship.

  SPEED
    improve speed.

  MISSILE
    effective against ground enemies.

  DOUBLE
    split your shots into two ways; this halves the strength of each 'shot',     remember!

  LASER
    Equip a penetrating laser.

  OPTION
    launch a support device that lazy-evaluates your ship's position.

  SHIELD
    create two shields that protects your ship. These shields will destroy anything that they contact (like enemy vessels or missiles), but they will be worn away, and they are completely destroyed by power ups!

** command-line options
   -r
    disable replay generation.
    without this, replays are saved automatically.

  -f
    attempt full screen mode.

  <filename>
    to playback a replay data, just pass its filename
    to the Monadius executable

** special thanks
  Monadius is written in Haskell, compiled by Glasgow Haskell Compiler 6.4 .
    The Haskell Home Page
    <http://www.haskell.org/>
    The Glasgow Haskell Compiler
    <http://www.haskell.org/ghc/>

  Utilizes OpenGL for rendering.
    <http://www.opengl.org/>

  Inspired by ugo-tool,
    <http://mclover.net/Program/Ugo.htm>
    Å@gradius2.com,
    <http://gradius2.com/>
  and the entire Gradius series.

** More information about us
  a scientist's toy box.
    <http://www.geocities.jp/takascience/index_e.html>