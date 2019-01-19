' GRIDFIGHTER 3D
'
' a virtual reality arcade experience
'
' (c) Jay Hammer, 2017-2018
'
' all code here by: Jay Hammer / desire ^ porta2note
'
' SETMODE.COM assembly by: hellmood / desire
'
' HUGE thanks to Scali, Reenigne, Trixter, Plasma & Mangis for optimization
' advice in versions post 1.2
'
' This source code is licensed under the Gnu Public License (GPL) v3. Looking
' up the text of this license is left as a homework exercise for the reader.
'
' Note that the compiled binaries I offer are licenced under CC-BY-NC. This
' means that versions of this game *you* make must adhere to the GPL but
' binaries *I* distribute on itch.io, Pouet, Demozoo, etc. do not. This also
' means that my prebuilt binaries can't be distributed in the same archive as
' the source code, except by me because it's my game and I can violate my own
' licenses if I want. If you disassemble one of my pre-compiled binaries and
' modify its code that way, I suppose it's possible to make a derivative work
' that's not GPL but it would then be under a non-commercial licence (the -NC
' part of CC) and you couldn't sell it for cash. Isn't licensing fun??
'
' Also note that I used both spellings of the word 'license' on purpose just
' to piss off the armchair lawyers even more.

' CHANGELOG / REVISION HISTORY:
' 0.F (15.10.2017) - game jam version
' 1.0 (17.10.2017) - first released version
'                  - load & save high scores
'                  - better difficulty progression
'                  - player does not spawn on an enemy & insta-die anymore
'                  - more sound effects & music added
'                  - slightly faster rendering (changed LINEs to PSETs)
'                  - graphics & splash screens tweaked
' 1.1 (18.10.2017) - fixed framerate cap from 5 to 12FPS
'                  - reset eyepos, eyeH on player respawn
'                  - added 7-8-9-0 keys to move the playfield x/y
'                  - added this changelog
'                  - changed initial MoveTrig for new framerates

' Changes in version 1.2:

' code rev. 16     - decoupled player movement from screen & enemy updates,
'                    removed all framerate limiters for DOS version.
'                    Added _LIMIT statements for QB64 (uncomment.)
' 0x17             - Changed player movement & grid square type detection
'                    from IF THEN ELSE to SELECT CASE
'                  - removed extra superfluous IF statements (e.g. key2$)
'                  - added LaserPos as separate entity
'                  - added fade effect to laser
'                  - changed reticle from circle to crosshairs
'                  - turned shadows back to red
'                  - enemies on grid 8 & reticle don't clash anymore
'                  - firing laser properly affects reticle grid square
'                  - explosions on PlayerPos.X/Y/Z do not leave player
'                    invisible in rare cases when player survives explosion
'                    (e.g. if enemy jumps into player square & is killed
'                    before collision detection. Note: this "bug" is left
'                    in to give player possibility of a 'lucky shot'.)
' 0x18             - removed LaserPos, Shot as separate entities
'                  - gave laser shots own grid color codes (24, 25)
'                  - made PlayerPos.C vary as w/RetColor
'                  - removed all UpdateAnim incriments from left eye renderer
' 0x19             - removed laser fade effect & all references to laserlow
'                    (grid colour 25 is no longer used)
'                  - fixed bug about updating the reticle square color after
'                    the player leaves the square when a shot's been fired
'                    blah blah blah this is too complicated to describe and
'                    I don't need to because it's gone now and will never
'                    return holyfucksticks
'                  - decrease enemy update delay when ramping difficulty up
'                    (VirtualFPS)
'                  - Added VirtualFPS to debug display
' 0x1A             - Deprecated code branch

' Changes in 1.3:

' 0x1A.8           - Changed PSET blocks back to LINEs and BOXes (i.e.
'                    reverted changes from 1.0. Apparently it's faster to
'                    draw a filled box than 9 PSETs. Who could've known??!1)
'                  - Fixed inconsistent rounding of pixel positions
' 0x1B (5d17)      - Changed Vpage copy to Vpage flip for HUGE speedup. Game
'                    is now playable on fast 386 & 486.
'                  - Fixed small 'jump' when player respawns
'                  - Added Vsync (DOS version) to prevent page flip
'                    from outrunning CLS, causing flicker
'                  - Added "novsync" option for people who enjoy headaches
'                  - Added "lowres" mode for 128kB EGA cards (changed all
'                    hardcoded SCREEN 9 to SCREEN VidMode)
'                  - Changed reticle to a triangle because moar triangles
'                  - Added extra secret debuggy keys
'                  - Added wait state in splash screen so that no-one gets
'                    anymore siezures
'                  - Framerate timer now stays accurate across deaths
'                  - Deliberately left "enemy burst mode" bug in, even though
'                    I know exactly what's causing it - you'll know it when
'                    you see it. It's amazing.
' 0x1C             - Deprecated branch. I forget what it did but it didn't
'                    work as well, so it's gone now.
' 0x1D (aka L3)    - Initial "smart clear", re-ran geometry calculation a
'                    second time
' 0x1E             - Moved calculation of smart clear box inside render loop
'                    to avoid second run of geometry calculation
'                  - Black grid squares on bottom plane are no longer drawn
'                    (oops)
'                  - Removed RecalcClear variable; always done now (faster)
' 0x1F             - Added FullClear flag if screen geometry is altered
'                    (what 0x1E taketh away...)
'                  - removed manual clear-box setting controls (former debug)
'                  - added preliminary VRdevice=4 mode for shutter glasses
'                    (not finished yet)
'                  - added DOUBLE-LOWRES from the command line because
'                    why the hell not. Note that VRdevice=4 forces DOUBLE-
'                    LOWRES. Text spacing needs to be tweaked a lot for
'                    both modes.
' 0x20             - Implimented Mangis's addition-based geometry calculations
'                    instead of my multiplication-based ones
' 0x21             - Adjusted new geometry calculations to work with variable
'                    screen sizes
'                  - Separated double-lowres and shutter glasses modes.
'                    VRdevice=4 now renders single-eye in full-screen (still
'                    320x200.)
'                  - replaced eyeoffset/offsetY controls which were accident-
'                    ally removed
' 0x22 (3f18) / v1.2F (1.3 beta preview)
'                  - changed vrdevice 4 to 9 in selection menu (only)
' 0x23 (Feb-July 2018) / 1.2F.x
'                  - changed LCD shutter mode to vrdevice=9 in code now too
'                  - added VR64 support (vrdevice 4)
'                  - fixed epileptic splash screen on fast machines
'                  - fixed splash screen button press detection on OS/X/Linux
' 0x24 (1A18) / v1.3
'                  - fixed .app working directory for high-score saves (OS/X)
'                  - updated Linux & OS/X ports to match the DOS version
'                  - separate Linux version for 32-bit CPUs (runs on 64-bit
'                    with ia32-libs installed.)
'                  - OS/X version now in .app bundle, with icon
'                  - OS/X version now supports Snow Leopard
' *** NOT DONE YET :
'                  - OS/X version now supports 32-bit CPUs

DECLARE SUB VRSetup ()
DECLARE SUB Splash ()
DECLARE SUB LaserShot ()
DECLARE SUB Fanfare ()
DECLARE SUB BAYSPLOSIONS ()
DECLARE SUB Loadhi ()
DECLARE SUB SaveHi ()

ON ERROR GOTO handler

DIM SHARED vrdevice, VidMode AS INTEGER
DIM SHARED Hscores(5) AS LONG
DIM SHARED errorflag AS INTEGER

' Save some memory & speed things up (hopefully)
DIM VW, VH, HVW, HalfHVW, ThreeHalfHVW, HVH AS INTEGER
DIM Spacing, LeyeX, ReyeX, eyez, RiftFudge AS INTEGER
DIM eyepos, eyeoffset, textoffset, eyeH, eyeY AS INTEGER
DIM TextCenterL, TextCenterR AS INTEGER
DIM a, B, zb, enemy, SpawnX, SpawnY, SpawnZ AS INTEGER
DIM PlayerIsDead, NumEnemies, NumKilled, Lives AS INTEGER
DIM score, ShowScore, DiffUp, BaseScore, ScoreUp AS INTEGER
DIM debug, bench, FrameNum, DeathRestart AS INTEGER
DIM ExtraLife, LaserGrid, RetColor, HSTag AS INTEGER
DIM Shot, offsetY, OGeyepos, OGeyeH, UpdateAnim AS INTEGER
DIM LaserUpdate, LaserHiC, rc, pc, ec, cc AS INTEGER
DIM TextCols, FullClear AS INTEGER
DIM Drawlines, leftline, rightline, topline, bottomline AS INTEGER

' DIM flatXL, flatXR, flatY AS ???

DIM VirtualFPS AS SINGLE
DIM LaserFPS AS SINGLE

IF INSTR(UCASE$(COMMAND$), "DOUBLE-LOWRES") THEN
    VW = 320
    VH = 200
    VidMode = 7
    TextCols = 40
ELSEIF INSTR(UCASE$(COMMAND$), "LOWRES") THEN
    VW = 640
    VH = 200
    VidMode = 8
    TextCols = 80
ELSE
    VW = 640 ' Vid width
    VH = 350 ' Vid height
    VidMode = 9
    TextCols = 80
END IF

HVH = VH / 2
HVW = VW / 2
HalfHVW = HVW / 2
ThreeHalfHVW = 3 * HVW / 2

IF INSTR(UCASE$(COMMAND$), "NOVSYNC") THEN
    Vsync = 0
ELSE
    Vsync = 1
END IF

VirtualFPS = .08
LaserFPS = .15

Loadhi
VRSetup

IF vrdevice = 99 THEN GOTO eop


IF vrdevice = 1 THEN
    Spacing = -4
    eyepos = 0
    eyeoffset = -20
    offsetY = 0
    textoffset = -3
    eyeH = -5
    eyez = -250
    eyeY = 128 - eyeH
    RiftFudge = 0
ELSEIF vrdevice = 2 THEN
    Spacing = -4
    eyepos = 0
    eyeoffset = 0
    offsetY = 0
    textoffset = 0
    eyeH = -5
    eyez = -400
    eyeY = 128 - eyeH
    RiftFudge = 20
ELSEIF vrdevice = 9 THEN
    VW = 640
    VH = 200
    VidMode = 7
    HVH = VH / 2
    HVW = VW / 2
    HalfHVW = HVW / 2
    ThreeHalfHVW = 3 * HVW / 2
    TextCols = 80

    Spacing = -4
    eyepos = 0
    eyeoffset = 0
    offsetY = 0
    textoffset = 0
    eyeH = -5
    eyez = -250
    eyeY = 128 - eyeH
    RiftFudge = 0
ELSE
    Spacing = -4
    eyepos = 0
    eyeoffset = 0
    offsetY = 0
    textoffset = 0
    eyeH = -5
    eyez = -250
    eyeY = 128 - eyeH
    RiftFudge = 0
END IF

OGeyepos = eyepos
OGeyeH = eyeH


TextCenterL = INT(TextCols / VW * ((HVW / 2) + RiftFudge)) + textoffset
TextCenterR = INT(TextCols / VW * ((3 * HVW / 2) - RiftFudge)) + textoffset

LeyeX = eyepos - Spacing
ReyeX = eyepos + Spacing

NumEnemies = 10

TYPE GridPt
    X AS SINGLE
    Y AS SINGLE
    Z AS SINGLE
    C AS INTEGER
END TYPE

DIM PlayerPos AS GridPt
DIM Enemies(NumEnemies) AS GridPt
DIM GridZCol(8) AS INTEGER
DIM Grid(8, 6, 6) AS GridPt

rc = 3
pc = 14
ec = 14
cc = 0

restart:
SCREEN VidMode, 0, 0, 0
Splash

Apage = 0
Vpage = 1

SCREEN VidMode, 0, Apage, Vpage
RANDOMIZE TIMER
CLS
pause$ = ""

' GRID STRUCTURE:
' GRID8 (rear) - z=320, x=-120 - 120, Y=-120 to 120 (y=120 reserved for
' shadow)
' GRID7 - z=280 ... etc. to Grid(1, z=0
' Gridn(x)=0 to 15 ... color of grid square
'          16 ... player ship
'          17 ... shadow
'          18 ... aiming reticle
'          24 ... lasershot
'          26-30 ... explosions
'          50-xx ... enemy types (currently only one)
'          etc.

GridZCol(1) = 15
GridZCol(2) = 11
GridZCol(3) = 13
GridZCol(4) = 7
GridZCol(5) = 9
GridZCol(6) = 3
GridZCol(7) = 8
GridZCol(8) = 1
RetColor = GridZCol(8)
PlayerPos.C = GridZCol(1)

LaserHiC = 15

FOR B = 1 TO 6
    FOR a = 1 TO 6
        Grid(1, a, B).C = GridZCol(1)
        Grid(2, a, B).C = GridZCol(2)
        Grid(3, a, B).C = GridZCol(3)
        Grid(4, a, B).C = GridZCol(4)
        Grid(5, a, B).C = GridZCol(5)
        Grid(6, a, B).C = GridZCol(6)
        Grid(7, a, B).C = GridZCol(7)
        Grid(8, a, B).C = GridZCol(8)

        Grid(1, a, B).X = (a * 40) - 140
        Grid(2, a, B).X = (a * 40) - 140
        Grid(3, a, B).X = (a * 40) - 140
        Grid(4, a, B).X = (a * 40) - 140
        Grid(5, a, B).X = (a * 40) - 140
        Grid(6, a, B).X = (a * 40) - 140
        Grid(7, a, B).X = (a * 40) - 140
        Grid(8, a, B).X = (a * 40) - 140

        Grid(1, a, B).Y = (B * 40) - 140
        Grid(2, a, B).Y = (B * 40) - 140
        Grid(3, a, B).Y = (B * 40) - 140
        Grid(4, a, B).Y = (B * 40) - 140
        Grid(5, a, B).Y = (B * 40) - 140
        Grid(6, a, B).Y = (B * 40) - 140
        Grid(7, a, B).Y = (B * 40) - 140
        Grid(8, a, B).Y = (B * 40) - 140

        Grid(1, a, B).Z = 0
        Grid(2, a, B).Z = 40
        Grid(3, a, B).Z = 80
        Grid(4, a, B).Z = 120
        Grid(5, a, B).Z = 160
        Grid(6, a, B).Z = 200
        Grid(7, a, B).Z = 240
        Grid(8, a, B).Z = 280
    NEXT a
NEXT B

FOR a = 1 TO 6
    Grid(1, a, 6).C = 0
    Grid(2, a, 6).C = 0
    Grid(3, a, 6).C = 0
    Grid(4, a, 6).C = 0
    Grid(5, a, 6).C = 0
    Grid(6, a, 6).C = 0
    Grid(7, a, 6).C = 0
    Grid(8, a, 6).C = 0
NEXT a

PlayerPos.X = 3
PlayerPos.Y = 2
PlayerPos.Z = 1
PlayerIsDead = 0

Grid(1, PlayerPos.X, PlayerPos.Y).C = 16
Grid(1, PlayerPos.X, 6).C = 17
Grid(8, PlayerPos.X, PlayerPos.Y).C = 18

FOR enemy = 1 TO 3
    Enemies(enemy).X = INT(RND * 6) + 1
    Enemies(enemy).Y = INT(RND * 5) + 1
    Enemies(enemy).Z = 8
    Enemies(enemy).C = 1

    Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = 50
    Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 17
NEXT enemy
 
FOR enemy = 4 TO NumEnemies
    Enemies(enemy).X = 0
    Enemies(enemy).Y = 0
    Enemies(enemy).Z = 0
    Enemies(enemy).C = 0
NEXT enemy

Lives = 3
score = 0
ShowScore = 0
DiffUp = 0
BaseScore = 100
NumKilled = 0
ScoreUp = BaseScore
SpawnProb = .98
MoveTrig = .08
debug = 0
bench = 0
Shot = 1

Fanfare

' Set initial geometry of clearing rectangle

Drawlines = 0

topline = 100
bottomline = 101
leftline = 318
rightline = 319

MaxVH = VH - 1

LaserUpdate = 0
UpdateAnim = 0
FrameNum = 0
FramerateStart = TIMER
MoveTimerStart = TIMER
LaserTimeStart = TIMER

DO
    _LIMIT 60 ' **** UNCOMMENT FOR QB64 ****
 
    LaserTime = TIMER
    LaserUpdate = 0

    IF LaserTime - LaserTimeStart > LaserFPS THEN
        LaserUpdate = 1
    END IF

    MoveTimer = TIMER
    FrameStart = TIMER
    IF UpdateAnim = 1 THEN
        IF PlayerIsDead > 0 THEN PlayerIsDead = PlayerIsDead + 1
        IF PlayerIsDead > 5 THEN ' Respawn player
    
            FOR enemy = 1 TO NumEnemies ' Player despawns enemies
                IF Enemies(enemy).X = 3 AND Enemies(enemy).Y = 2 AND Enemies(enemy).Z = 1 THEN
                    Enemies(enemy).X = 0
                    Enemies(enemy).Y = 0
                    Enemies(enemy).Z = 0
                    Enemies(enemy).C = 0
                END IF
            NEXT enemy
    
            Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
            Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0
            Grid(8, PlayerPos.X, PlayerPos.Y).C = GridZCol(8)
    
            PlayerPos.X = 3
            PlayerPos.Y = 2
            PlayerPos.Z = 1
            PlayerPos.C = GridZCol(PlayerPos.Z)
            PlayerIsDead = 0
            eyepos = OGeyepos
            eyeH = OGeyeH
            LeyeX = eyepos - Spacing
            ReyeX = eyepos + Spacing

            Grid(1, PlayerPos.X, PlayerPos.Y).C = 16
            Grid(1, PlayerPos.X, 6).C = 17
            Grid(8, PlayerPos.X, PlayerPos.Y).C = 18

            DeathRestart = 2

        END IF

    END IF

    ' render all grids
    'IF Vsync THEN WAIT &H3DA, 8 ' **** COMMENT OUT FOR QB64 ****

    ' Clear the entire screen instead of the smart clear box in some cases
 
    IF FullClear THEN
        topline = 0
        leftline = 0
        rightline = VW - 1
        bottomline = MaxVH
        FullClear = FullClear + 1
        IF FullClear = 3 THEN FullClear = 0 ' needed to ensure all pages cleared
 
    END IF

    LINE (leftline, topline)-(rightline, bottomline), cc, BF ' smart clear

    FOR zb = 8 TO 1 STEP -1
        baseXL = (((Grid(zb, 1, 1).X - ReyeX) / (Grid(zb, 1, 1).Z - eyez)) * HVW) + HalfHVW + RiftFudge
        baseXR = (((Grid(zb, 1, 1).X - LeyeX) / (Grid(zb, 1, 1).Z - eyez)) * HVW) + ThreeHalfHVW - RiftFudge
        baseY = (((Grid(zb, 1, 1).Y - eyeH) / (Grid(zb, 1, 1).Z - eyez)) * VH) + HVH
   
        deltaX = ((((Grid(zb, 2, 1).X - ReyeX) / (Grid(zb, 2, 1).Z - eyez)) * HVW) + HalfHVW + RiftFudge) - baseXL
        DeltaY = ((((Grid(zb, 1, 2).Y - eyeH) / (Grid(zb, 1, 2).Z - eyez)) * VH) + HVH) - baseY
   
        TotalDeltaY = 0
   
        FOR B = 1 TO 6
     
            flatY = baseY + TotalDeltaY + offsetY
            TotalDeltaY = TotalDeltaY + DeltaY
     
            TotalDeltaX = 0

            FOR a = 1 TO 6
       
                flatXL = baseXL + TotalDeltaX + eyeoffset
                flatXR = baseXR + TotalDeltaX + eyeoffset
                TotalDeltaX = TotalDeltaX + deltaX

                IF zb = 1 THEN
                    IF a = 1 AND B = 1 THEN
                        topline = flatY - 7
                        leftline = flatXL - 7
                    ELSEIF a = 6 AND B = 6 THEN
                        rightline = flatXR + 7
                        bottomline = flatY + 7
                    END IF
                END IF
       
                ' LEFT EYE DRAW ROUTINE

                IF (flatXL > 0) AND (flatXL < HVW) THEN
                                         
                    SELECT CASE Grid(zb, a, B).C
                   
                        CASE 1 TO 15 ' draw grid square
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), Grid(zb, a, B).C, BF
        
                        CASE 16 ' draw player
                            IF PlayerIsDead = 0 THEN
                                LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), PlayerPos.C, BF
                                LINE (flatXL, flatY - 5)-(flatXL + 4, flatY + 4), pc
                                LINE (flatXL, flatY - 5)-(flatXL - 4, flatY + 4), pc
                                LINE (flatXL + 4, flatY + 4)-(flatXL - 4, flatY + 4), pc
                            END IF

                        CASE 17 ' draw shadow
                            CIRCLE (flatXL, flatY), 4, 4, , , .25

                        CASE 18 ' draw aiming reticle
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), RetColor, BF
                   
                            LINE (flatXL - 4, flatY - 2)-(flatXL + 4, flatY - 2), rc
                            LINE (flatXL - 3, flatY - 1)-(flatXL, flatY + 4), rc
                            LINE (flatXL + 3, flatY - 1)-(flatXL, flatY + 4), rc

                        CASE 24 ' Lasershot
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), LaserHiC, BF
            
                        CASE 50 ' draw enemy
                            LINE (flatXL - 3, flatY - 3)-(flatXL + 3, flatY + 3), ec, BF

                        CASE 26 ' EXPLOSION !
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXL, flatY), 3, 15
                        CASE 27
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXL, flatY), 4, 15
                        CASE 28
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXL, flatY), 5, 14
                        CASE 29
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXL, flatY), 6, 12
                        CASE 30
                            LINE (flatXL - 1, flatY - 1)-(flatXL + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXL, flatY), 7, 4

                    END SELECT
                END IF

                ' RIGHT EYE DRAW ROUTINE
       
                IF (flatXR > HVW) AND (flatXR < VW) AND vrdevice <> 9 THEN
         
                    SELECT CASE Grid(zb, a, B).C
                   
                        CASE 1 TO 15 ' draw grid square
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), Grid(zb, a, B).C, BF

                        CASE 16 ' draw player
                            IF PlayerIsDead = 0 THEN
                                LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), PlayerPos.C, BF
                                LINE (flatXR, flatY - 5)-(flatXR + 4, flatY + 4), pc
                                LINE (flatXR, flatY - 5)-(flatXR - 4, flatY + 4), pc
                                LINE (flatXR + 4, flatY + 4)-(flatXR - 4, flatY + 4), pc
                            END IF

                        CASE 17 ' draw shadow
                            CIRCLE (flatXR, flatY), 4, 4, , , .25

                        CASE 18 ' draw aiming reticle
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), RetColor, BF
             
                            LINE (flatXR - 4, flatY - 2)-(flatXR + 4, flatY - 2), rc
                            LINE (flatXR - 3, flatY - 1)-(flatXR, flatY + 4), rc
                            LINE (flatXR + 3, flatY - 1)-(flatXR, flatY + 4), rc

                        CASE 50 ' draw enemy
                            LINE (flatXR - 3, flatY - 3)-(flatXR + 3, flatY + 3), ec, BF
           
                        CASE 24 ' Lasershot
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), LaserHiC, BF
         
                        CASE 26 ' EXPLOSION !
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXR, flatY), 3, 15
                        CASE 27
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXR, flatY), 4, 15
                        CASE 28
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXR, flatY), 5, 14
                        CASE 29
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXR, flatY), 6, 12
                        CASE 30
                            LINE (flatXR - 1, flatY - 1)-(flatXR + 1, flatY + 1), Grid(zb, a, B).C, BF
                            CIRCLE (flatXR, flatY), 7, 4

                    END SELECT
                END IF
    
                SELECT CASE Grid(zb, a, B).C ' update some animation ticks (needed
                    ' to move this outside the renderer
                    ' for vrdevice=9 )
                    CASE 24
                        IF LaserUpdate = 1 THEN Grid(zb, a, B).C = GridZCol(zb)
                    CASE 26
                        IF UpdateAnim = 1 THEN Grid(zb, a, B).C = 27
                    CASE 27
                        IF UpdateAnim = 1 THEN Grid(zb, a, B).C = 28
                    CASE 28
                        IF UpdateAnim = 1 THEN Grid(zb, a, B).C = 29
                    CASE 29
                        IF UpdateAnim = 1 THEN Grid(zb, a, B).C = 30
                    CASE 30
                        IF UpdateAnim = 1 THEN
                            Grid(zb, a, B).C = GridZCol(zb)
                            IF zb = 8 AND PlayerPos.X = a AND PlayerPos.Y = B THEN Grid(zb, a, B).C = 18
                        END IF
                END SELECT

            NEXT a
        NEXT B
    NEXT zb
       
    IF topline < 0 THEN topline = 0
    IF leftline < 0 THEN leftline = 0
    IF rightline > (VW - 1) THEN rightline = VW - 1
    IF bottomline > MaxVH THEN bottomline = MaxVH

    IF Drawlines THEN
        LINE (0, topline)-(639, topline), 15
        LINE (0, bottomline)-(639, bottomline), 15
        LINE (leftline, 0)-(leftline, VH), 15
        LINE (rightline, 0)-(rightline, VH), 15
   
        LOCATE 1, 60
        COLOR 15
        PRINT leftline; topline; "-"; rightline; bottomline
    END IF
 
    IF LaserUpdate = 1 THEN

        RetColor = GridZCol(8)
        PlayerPos.C = GridZCol(PlayerPos.Z)

    END IF

    IF Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C < 16 THEN Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16

    IF debug = 1 THEN
        LOCATE 1, 1
        COLOR 15
        PRINT "Spacing"; Spacing; "LeyeX"; LeyeX; "ReyeX"; ReyeX; "Zoom"; eyez; "RiftFudge"; RiftFudge; "SpawnP";
        PRINT USING " .##"; SpawnProb;
        PRINT " MoveP";
        PRINT USING " .##"; MoveTrig;
        PRINT " Kills"; NumKilled
        PRINT "Enemy update time";
        PRINT USING " .## s"; VirtualFPS
        FOR enemy = 1 TO NumEnemies
            PRINT "Enemy "; enemy; "X "; Enemies(enemy).X; "Y "; Enemies(enemy).Y; "Z "; Enemies(enemy).Z; Enemies(enemy).C
        NEXT enemy

    END IF
 

    IF DeathRestart = 2 THEN
        DeathRestart = 0
   
        SCREEN VidMode, 0, Vpage, Vpage
        COLOR 15

        LOCATE 12, (TextCenterL - 3)
        PRINT "YOU DIED"
        LOCATE 13, (TextCenterL - 4)
        PRINT "PRESS SPACE"

        IF vrdevice <> 9 THEN
            LOCATE 12, (TextCenterR - 3)
            PRINT "YOU DIED"
            LOCATE 13, (TextCenterR - 4)
            PRINT "PRESS SPACE"
        END IF

        Fanfare

        DO
            _LIMIT 60 ' **** UNCOMMENT FOR QB64 ****
            PressSpace$ = INKEY$
            IF PressSpace$ = CHR$(27) THEN
                key$ = CHR$(27)
                PressSpace$ = ""
                GOTO iquit
            END IF
        LOOP UNTIL PressSpace$ = " "
        FrameNum = 0
        FramerateStart = TIMER
        FullClear = 1
    END IF

    IF ShowScore = 0 THEN
        COLOR 15
        LOCATE 20, (TextCenterL - 6)
        PRINT "Player 1 "; score
   
        IF vrdevice <> 9 THEN
            LOCATE 20, (TextCenterR - 6)
            PRINT "Player 1 "; score
        END IF

    ELSEIF ShowScore > 0 THEN
        IF UpdateAnim = 1 THEN
            COLOR INT(RND * 14) + 1
            ShowScore = ShowScore + 1
        END IF
        LOCATE 20, (TextCenterL - 10)
        PRINT "Spawn rate increased!"
   
        IF vrdevice <> 9 THEN
            LOCATE 20, (TextCenterR - 10)
            PRINT "Spawn rate increased!"
        END IF

        IF ShowScore = 11 THEN ShowScore = 0
    END IF

    IF ExtraLife = 0 AND DiffUp = 0 THEN
        COLOR 15
        LOCATE 21, (TextCenterL - 3)
        PRINT "Lives "; Lives
   
        IF vrdevice <> 9 THEN
            LOCATE 21, (TextCenterR - 3)
            PRINT "Lives "; Lives
        END IF
 
    ELSEIF ExtraLife > 0 AND DiffUp = 0 THEN
        IF UpdateAnim = 1 THEN
            COLOR INT(RND * 14) + 1
            ExtraLife = ExtraLife + 1
        END IF
        LOCATE 21, (TextCenterL - 4)
        PRINT "Extra life!"
   
        IF vrdevice <> 9 THEN
            LOCATE 21, (TextCenterR - 4)
            PRINT "Extra life!"
        END IF

        IF ExtraLife = 11 THEN ExtraLife = 0
    ELSEIF DiffUp > 0 THEN
        IF UpdateAnim = 1 THEN
            DiffUp = DiffUp + 1
            COLOR INT(RND * 14) + 1
        END IF
        LOCATE 21, (TextCenterL - 6)
        PRINT "Faster enemies!"
   
        IF vrdevice <> 9 THEN
            LOCATE 21, (TextCenterR - 6)
            PRINT "Faster enemies!"
        END IF

        IF DiffUp = 11 THEN DiffUp = 0
    END IF

    Apage = ABS(Apage - 1)
    Vpage = ABS(Vpage - 1)
    SCREEN VidMode, 0, Apage, Vpage

    key$ = UCASE$(INKEY$)

    iquit:

    SELECT CASE key$

        ' move world
   
        CASE "."
            eyez = eyez + 50
            FullClear = 1
        CASE ","
            eyez = eyez - 50
            FullClear = 1
        CASE "["
            Spacing = Spacing - 1
            FullClear = 1
        CASE "]"
            Spacing = Spacing + 1
            FullClear = 1
        CASE "("
            RiftFudge = RiftFudge + 2
            FullClear = 1
        CASE ")"
            RiftFudge = RiftFudge - 2
            FullClear = 1
        CASE "7"
            eyeoffset = eyeoffset - 8
            textoffset = textoffset - 1
            TextCenterL = INT(TextCols / VW * ((HVW / 2) + RiftFudge)) + textoffset
            TextCenterR = INT(TextCols / VW * ((3 * HVW / 2) - RiftFudge)) + textoffset
            FullClear = 1
        CASE "0"
            eyeoffset = eyeoffset + 8
            textoffset = textoffset + 1
            TextCenterL = INT(TextCols / VW * ((HVW / 2) + RiftFudge)) + textoffset
            TextCenterR = INT(TextCols / VW * ((3 * HVW / 2) - RiftFudge)) + textoffset
            FullClear = 1
        CASE "9"
            offsetY = offsetY - 5
            FullClear = 1
        CASE "8"
            offsetY = offsetY + 5
            FullClear = 1

            ' debug keys

        CASE "~"
            debug = ABS(debug - 1)
            FullClear = 1
        CASE "!"
            bench = ABS(bench - 1)
        CASE "#"
            rc = rc + 1
            IF rc = 16 THEN rc = 0
        CASE "$"
            pc = pc + 1
            IF pc = 16 THEN pc = 0
        CASE "%"
            ec = ec + 1
            IF ec = 16 THEN ec = 0
        CASE "@"
            cc = ABS(cc - 1)
        CASE "^"
            Drawlines = ABS(Drawlines - 1)
   
            ' move player
    
        CASE CHR$(0) + CHR$(75)
            IF PlayerPos.X - 1 >= 1 AND PlayerIsDead = 0 THEN
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0
                IF Grid(8, PlayerPos.X, PlayerPos.Y).C = 18 THEN
                    IF RetColor = LaserHiC THEN
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = 24
                    ELSE
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = GridZCol(8)
                    END IF
                END IF
   
                PlayerPos.X = PlayerPos.X - 1
                eyepos = eyepos - 2
   
                PlayerPos.C = GridZCol(PlayerPos.Z)
                RetColor = 1

                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 17

                IF PlayerPos.Z <> 8 AND Grid(8, PlayerPos.X, PlayerPos.Y).C <> 50 THEN Grid(8, PlayerPos.X, PlayerPos.Y).C = 18
       
            END IF

        CASE CHR$(0) + CHR$(77)
            IF PlayerPos.X + 1 <= 6 AND PlayerIsDead = 0 THEN
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0
                IF Grid(8, PlayerPos.X, PlayerPos.Y).C = 18 THEN
                    IF RetColor = LaserHiC THEN
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = 24
                    ELSE
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = GridZCol(8)
                    END IF
                END IF

                PlayerPos.X = PlayerPos.X + 1
                eyepos = eyepos + 2
  
                PlayerPos.C = GridZCol(PlayerPos.Z)
                RetColor = 1

                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 17

                IF PlayerPos.Z <> 8 AND Grid(8, PlayerPos.X, PlayerPos.Y).C <> 50 THEN Grid(8, PlayerPos.X, PlayerPos.Y).C = 18
       
            END IF
   
        CASE CHR$(0) + CHR$(72)
            IF PlayerPos.Y - 1 >= 1 AND PlayerIsDead = 0 THEN
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0
                IF Grid(8, PlayerPos.X, PlayerPos.Y).C = 18 THEN
                    IF RetColor = LaserHiC THEN
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = 24
                    ELSE
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = GridZCol(8)
                    END IF
                END IF
 
                PlayerPos.Y = PlayerPos.Y - 1
                eyeH = eyeH - 2
 
                PlayerPos.C = GridZCol(PlayerPos.Z)
                RetColor = 1

                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 17

                IF PlayerPos.Z <> 8 AND Grid(8, PlayerPos.X, PlayerPos.Y).C <> 50 THEN Grid(8, PlayerPos.X, PlayerPos.Y).C = 18
       
            END IF
   
        CASE CHR$(0) + CHR$(80)
            IF PlayerPos.Y + 1 <= 5 AND PlayerIsDead = 0 THEN
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0
                IF Grid(8, PlayerPos.X, PlayerPos.Y).C = 18 THEN
                    IF RetColor = LaserHiC THEN
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = 24
                    ELSE
                        Grid(8, PlayerPos.X, PlayerPos.Y).C = GridZCol(8)
                    END IF
                END IF

                PlayerPos.Y = PlayerPos.Y + 1
                eyeH = eyeH + 2

                PlayerPos.C = GridZCol(PlayerPos.Z)
                RetColor = 1

                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 17

                IF PlayerPos.Z <> 8 AND Grid(8, PlayerPos.X, PlayerPos.Y).C <> 50 THEN Grid(8, PlayerPos.X, PlayerPos.Y).C = 18
     
            END IF

        CASE "A"
            IF PlayerPos.Z + 1 <= 8 AND PlayerIsDead = 0 THEN
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0

                PlayerPos.Z = PlayerPos.Z + 1

                PlayerPos.C = GridZCol(PlayerPos.Z)
                RetColor = 1
       
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 17
     
            END IF

        CASE "Z"
            IF PlayerPos.Z - 1 >= 1 AND PlayerIsDead = 0 THEN
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = GridZCol(PlayerPos.Z)
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 0

                PlayerPos.Z = PlayerPos.Z - 1

                PlayerPos.C = GridZCol(PlayerPos.Z)
                RetColor = 1
      
                Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 16
                Grid(PlayerPos.Z, PlayerPos.X, 6).C = 17

                Grid(8, PlayerPos.X, PlayerPos.Y).C = 18
            END IF

        CASE " "
            IF PlayerIsDead = 0 THEN

                LaserShot
                LaserTimeStart = TIMER
                LaserTime = 0

                FOR LaserGrid = 1 TO 8
                    IF Grid(LaserGrid, PlayerPos.X, PlayerPos.Y).C < 16 THEN
                        Grid(LaserGrid, PlayerPos.X, PlayerPos.Y).C = 24
                    END IF
                NEXT LaserGrid

                RetColor = LaserHiC
                PlayerPos.C = LaserHiC

                FOR enemy = 1 TO NumEnemies
                    IF PlayerPos.X = Enemies(enemy).X AND PlayerPos.Y = Enemies(enemy).Y THEN ' AND PlayerPos.Z < Enemies(enemy).Z THEN
                        score = score + ScoreUp
                        ScoreUp = INT(ScoreUp * 2.5)
                        NumKilled = NumKilled + 1
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = 26
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 0

                        Enemies(enemy).X = 0
                        Enemies(enemy).Y = 0
                        Enemies(enemy).Z = 0
                        Enemies(enemy).C = 0

                        BAYSPLOSIONS

                        IF NumKilled MOD 10 = 0 AND SpawnProb > .5 THEN
                            PLAY "o2 mb t160 l64 fp64fp64fp64"
                            ShowScore = 1
                            SpawnProb = SpawnProb - .04
                        END IF
                        IF NumKilled MOD 12 = 0 THEN
                            PLAY "o3 mb t160 l32 cg>c"
                            Lives = Lives + 1
                            ExtraLife = 1
                        END IF
                        IF NumKilled MOD 17 = 0 AND MoveTrig < .3 THEN
                            MoveTrig = MoveTrig + .03
                            IF VirtualFPS > .03 THEN VirtualFPS = VirtualFPS - .01
                            DiffUp = 1
                            PLAY "o2 mb t160 l24 f#dc"
                        END IF

                    END IF

                NEXT enemy
                ScoreUp = BaseScore
            END IF
    END SELECT

    FOR enemy = 1 TO NumEnemies
        IF PlayerPos.X = Enemies(enemy).X AND PlayerPos.Y = Enemies(enemy).Y AND PlayerPos.Z = Enemies(enemy).Z AND PlayerIsDead = 0 THEN
            BAYSPLOSIONS
            PlayerIsDead = 1
            Lives = Lives - 1
            DeathRestart = 1

            Enemies(enemy).X = 0
            Enemies(enemy).Y = 0
            Enemies(enemy).Z = 0
            Enemies(enemy).C = 0

            Grid(PlayerPos.Z, PlayerPos.X, PlayerPos.Y).C = 26
        END IF
    NEXT enemy

    UpdateAnim = 0

    IF MoveTimer - MoveTimerStart > VirtualFPS THEN
        UpdateAnim = 1
        MoveTimerStart = TIMER
     
        FOR enemy = 1 TO NumEnemies
            MoveProb = RND
            IF Enemies(enemy).C = 1 THEN
                IF MoveProb < MoveTrig THEN
                    IF Enemies(enemy).Z > PlayerPos.Z THEN
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = GridZCol(Enemies(enemy).Z)
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 0
                        IF Enemies(enemy).Z = 8 AND Enemies(enemy).X = PlayerPos.X AND Enemies(enemy).Y = PlayerPos.Y THEN Grid(8, Enemies(enemy).X, Enemies(enemy).Y).C = 18
                        Enemies(enemy).Z = Enemies(enemy).Z - 1
                    ELSEIF Enemies(enemy).Z < PlayerPos.Z THEN
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = GridZCol(Enemies(enemy).Z)
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 0
                        Enemies(enemy).Z = Enemies(enemy).Z + 1

                    ELSEIF Enemies(enemy).Z = PlayerPos.Z THEN
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = GridZCol(Enemies(enemy).Z)
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 0

                        IF MoveProb < (MoveTrig / 2) THEN
                            IF Enemies(enemy).X < PlayerPos.X THEN Enemies(enemy).X = Enemies(enemy).X + 1
                            IF Enemies(enemy).X > PlayerPos.X THEN Enemies(enemy).X = Enemies(enemy).X - 1
                        ELSE
                            IF Enemies(enemy).Y < PlayerPos.Y THEN Enemies(enemy).Y = Enemies(enemy).Y + 1
                            IF Enemies(enemy).Y > PlayerPos.Y THEN Enemies(enemy).Y = Enemies(enemy).Y - 1
                        END IF
                    END IF
                END IF

                Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = 50
                Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 17

            ELSEIF Enemies(enemy).C = 0 THEN

                IF MoveProb > SpawnProb THEN
                    SpawnX = INT(RND * 6) + 1
                    SpawnY = INT(RND * 5) + 1
                    SpawnZ = 8

                    ' prevent spawnkills
                    IF SpawnX <> PlayerPos.X OR SpawnY <> PlayerPos.Y OR SpawnZ <> PlayerPos.Z THEN
                        Enemies(enemy).X = INT(RND * 6) + 1
                        Enemies(enemy).Y = INT(RND * 5) + 1
                        Enemies(enemy).Z = 8
                        Enemies(enemy).C = 1

                        Grid(Enemies(enemy).Z, Enemies(enemy).X, Enemies(enemy).Y).C = 50
                        Grid(Enemies(enemy).Z, Enemies(enemy).X, 6).C = 17
                    END IF
                END IF
            END IF

        NEXT enemy
    END IF

    LeyeX = eyepos - Spacing
    ReyeX = eyepos + Spacing

    FrameNum = FrameNum + 1

LOOP UNTIL key$ = CHR$(27) OR Lives = 0 AND PlayerIsDead = 5

FramerateEnd = TIMER
fps = FrameNum / (FramerateEnd - FramerateStart)
HSTag = 0
COLOR 15

SCREEN VidMode, 0, 0, 0

COLOR 15

IF PlayerIsDead = 5 THEN
    LOCATE 11, (TextCenterL - 3)
    PRINT "YOU DIED"
 
    IF vrdevice <> 9 THEN LOCATE 11, (TextCenterR - 3): PRINT "YOU DIED"
ELSE
    LOCATE 11, (TextCenterL - 3)
    PRINT "YOU QUIT"

    IF vrdevice <> 9 THEN LOCATE 11, (TextCenterR - 3): PRINT "YOU QUIT"
END IF
        
LOCATE 12, (TextCenterL - 4)
PRINT "Score"; score
LOCATE 13, (TextCenterL - 8)
PRINT NumKilled; "enemies killed!"
LOCATE 16, (TextCenterL - 8)
PRINT "SPACE - PLAY AGAIN"
LOCATE 17, (TextCenterL - 3)
PRINT "Q TO QUIT"


IF vrdevice <> 9 THEN
    LOCATE 12, (TextCenterR - 4)
    PRINT "Score"; score
    LOCATE 13, (TextCenterR - 8)
    PRINT NumKilled; "enemies killed!"
    LOCATE 16, (TextCenterR - 8)
    PRINT "SPACE - PLAY AGAIN"
    LOCATE 17, (TextCenterR - 3)
    PRINT "Q TO QUIT"
END IF

IF bench = 1 OR debug = 1 THEN
    LOCATE 2, TextCols - 10
    IF vrdevice = 9 THEN LOCATE 2, 30
    PRINT USING "##.##"; fps;
    PRINT " FPS"
END IF

SCREEN VidMode, 0, 1, 0

IF score <= Hscores(5) THEN
    COLOR 15
    LOCATE 11, (TextCenterL - 4)
    PRINT "HIGH SCORES"
    LOCATE 13, (TextCenterL - 4)
    COLOR 15
    PRINT "ASS -"; Hscores(1)
    LOCATE 14, (TextCenterL - 4)
    COLOR 11
    PRINT "ASS -"; Hscores(2)
    LOCATE 15, (TextCenterL - 4)
    COLOR 10
    PRINT "ASS -"; Hscores(3)
    LOCATE 16, (TextCenterL - 4)
    COLOR 2
    PRINT "ASS -"; Hscores(4)
    LOCATE 17, (TextCenterL - 4)
    COLOR 8
    PRINT "BUT -"; Hscores(5)


    IF vrdevice <> 9 THEN
        COLOR 15
        LOCATE 11, (TextCenterR - 4)
        PRINT "HIGH SCORES"
        LOCATE 13, (TextCenterR - 4)
        COLOR 15
        PRINT "ASS -"; Hscores(1)
        LOCATE 14, (TextCenterR - 4)
        COLOR 11
        PRINT "ASS -"; Hscores(2)
        LOCATE 15, (TextCenterR - 4)
        COLOR 10
        PRINT "ASS -"; Hscores(3)
        LOCATE 16, (TextCenterR - 4)
        COLOR 2
        PRINT "ASS -"; Hscores(4)
        LOCATE 17, (TextCenterR - 4)
        COLOR 8
        PRINT "BUT -"; Hscores(5)
    END IF
END IF

IF bench = 1 OR debug = 1 THEN
    LOCATE 2, TextCols - 10
    PRINT USING "##.##"; fps;
    PRINT " FPS"
END IF

DO
 
    IF HSTag = 0 THEN
        SCREEN VidMode, 0, 0, 0
    ELSEIF HSTag = 1 THEN
        SCREEN VidMode, 0, 1, 1
    END IF
 
    Atime = TIMER
    DO
        _LIMIT 30 ' **** UNCOMMENT FOR QB64 ****
        pause$ = INKEY$
        Btime = TIMER
    LOOP UNTIL Btime - Atime > 2 OR pause$ = CHR$(27) OR pause$ = " " OR UCASE$(pause$) = "Q"

    HSTag = ABS(HSTag - 1)
LOOP UNTIL pause$ = " " OR pause$ = CHR$(27) OR UCASE$(pause$) = "Q"


FinalExit = 0

IF score > Hscores(5) THEN

    SCREEN VidMode, 0, 1, 1
    CLS

    COLOR 15

    LOCATE 12, (TextCenterL - 5)
    PRINT "You got a high"
    LOCATE 13, (TextCenterL - 6)
    PRINT "score of"; score; "!"

    LOCATE 15, (TextCenterL - 8)
    PRINT "Enter your initials!"

    LOCATE 16, (TextCenterL - 2)
    PRINT ">"


    IF vrdevice <> 9 THEN
        LOCATE 12, (TextCenterR - 5)
        PRINT "You got a high"
        LOCATE 13, (TextCenterR - 6)
        PRINT "score of"; score; "!"

        LOCATE 15, (TextCenterR - 8)
        PRINT "Enter your initials!"

        LOCATE 16, (TextCenterR - 2)
        PRINT ">"
    END IF

    DO

        _LIMIT 30 ' **** UNCOMMENT FOR QB64 ****
        COLOR INT(RND * 15) + 1
        LOCATE 10, (TextCenterL - 6)
        PRINT "CONGRATULATIONS!"

        IF vrdevice <> 9 THEN LOCATE 10, (TextCenterR - 6): PRINT "CONGRATULATIONS!"

        IF FinalExit = 1 THEN
            LOCATE 16, (TextCenterL)
            PRINT "A"
     
            IF vrdevice <> 9 THEN LOCATE 16, (TextCenterR): PRINT "A"
        ELSEIF FinalExit = 2 THEN
            LOCATE 16, (TextCenterL)
            PRINT "AS"
            IF vrdevice <> 9 THEN LOCATE 16, (TextCenterR): PRINT "AS"
        ELSEIF FinalExit = 3 THEN
            LOCATE 16, (TextCenterL)
            PRINT "ASS"
            IF vrdevice <> 9 THEN LOCATE 16, (TextCenterR): PRINT "ASS"
        END IF

        Ass$ = INKEY$
        IF Ass$ <> "" THEN FinalExit = FinalExit + 1


    LOOP UNTIL FinalExit = 4

    IF score > Hscores(1) THEN
        Hscores(5) = Hscores(4)
        Hscores(4) = Hscores(3)
        Hscores(3) = Hscores(2)
        Hscores(2) = Hscores(1)
        Hscores(1) = INT(score)
    ELSEIF score > Hscores(2) THEN
        Hscores(5) = Hscores(4)
        Hscores(4) = Hscores(3)
        Hscores(3) = Hscores(2)
        Hscores(2) = INT(score)
    ELSEIF score > Hscores(3) THEN
        Hscores(5) = Hscores(4)
        Hscores(4) = Hscores(3)
        Hscores(3) = INT(score)
    ELSEIF score > Hscores(4) THEN
        Hscores(5) = Hscores(4)
        Hscores(4) = INT(score)
    ELSEIF score > Hscores(5) THEN
        Hscores(5) = INT(score)
    END IF

    SaveHi

    CLS
    COLOR 15
    LOCATE 11, (TextCenterL - 4)
    PRINT "HIGH SCORES"
    LOCATE 13, (TextCenterL - 4)
    COLOR 15
    PRINT "ASS -"; Hscores(1)
    LOCATE 14, (TextCenterL - 4)
    COLOR 11
    PRINT "ASS -"; Hscores(2)
    LOCATE 15, (TextCenterL - 4)
    COLOR 10
    PRINT "ASS -"; Hscores(3)
    LOCATE 16, (TextCenterL - 4)
    COLOR 2
    PRINT "ASS -"; Hscores(4)
    LOCATE 17, (TextCenterL - 4)
    COLOR 8
    PRINT "BUT -"; Hscores(5)


    COLOR 2
    LOCATE 19, (TextCenterL - 7)
    PRINT "(press ESC to quit)"

    IF vrdevice <> 9 THEN
        COLOR 15
        LOCATE 11, (TextCenterR - 4)
        PRINT "HIGH SCORES"
        LOCATE 13, (TextCenterR - 4)
        COLOR 15
        PRINT "ASS -"; Hscores(1)
        LOCATE 14, (TextCenterR - 4)
        COLOR 11
        PRINT "ASS -"; Hscores(2)
        LOCATE 15, (TextCenterR - 4)
        COLOR 10
        PRINT "ASS -"; Hscores(3)
        LOCATE 16, (TextCenterR - 4)
        COLOR 2
        PRINT "ASS -"; Hscores(4)
        LOCATE 17, (TextCenterR - 4)
        COLOR 8
        PRINT "BUT -"; Hscores(5)

     
        LOCATE 19, (TextCenterR - 7)
        PRINT "(press ESC to quit)"
    END IF

ELSE
    GOTO nohighscore
END IF

HSTag = 1

DO

    IF HSTag = 0 THEN
        SCREEN VidMode, 0, 0, 0
    ELSEIF HSTag = 1 THEN
        SCREEN VidMode, 0, 1, 1
    END IF

    Atime = TIMER
    DO
        _LIMIT 30 ' **** UNCOMMENT FOR QB64 ****
        pause$ = INKEY$
        Btime = TIMER
    LOOP UNTIL Btime - Atime > 2 OR pause$ = " " OR UCASE$(pause$) = "Q" OR pause$ = CHR$(27)

    HSTag = ABS(HSTag - 1)
LOOP WHILE pause$ = ""

nohighscore:

IF pause$ <> CHR$(27) AND UCASE$(pause$) <> "Q" THEN
    SCREEN VidMode, 0, 1, 0
    CLS
    GOTO restart
END IF

GOTO eop

handler:
errorflag = ERR
RESUME NEXT

eop:

SUB BAYSPLOSIONS

PLAY "o2 mb l64 t150 c<c<cl32c"

END SUB

SUB Fanfare

Fandumb = INT(RND * 3)
IF Fandumb = 0 THEN
    PLAY "mb o1l16t150 CP16<C>CP16C<CP16>D#P16<D#P16>D#FP16<A#>L4C"
ELSEIF Fandumb = 1 THEN
    PLAY "mb o0l16t150 f>cf cf>c <f>cf cf>c p16 c<g#gd#l4f"
ELSEIF Fandumb = 2 THEN
    PLAY "mb o1l16t150 c<c>gp16 c<c>gc<g>cgp16 c<a#>d#<a#>f<g>d#<g>d<c>gl4c"
END IF

END SUB

SUB LaserShot

PLAY "o5 mb l64 t240 c<gc<gc<gc<gc<gc"

END SUB

SUB Loadhi

DIM hsfile(5) AS STRING

' create highscore file if it doesn't already exist
' OPEN "GRID3D.HI" FOR BINARY AS 1
' CLOSE 1

' *** OS/X HACK: detect if running from .app & change working directory
' *** UNCOMMENT FOR QB64 ***
' Startdir$ = LCASE$(_CWD$)
' PRINT "Current directory", Startdir$
' IF INSTR(Startdir$, ".app") THEN
' CHDIR "../Resources"
' PRINT "Changed directory to ", _CWD$
' END IF
' PRINT "Loading highscores..."
' DO: LOOP WHILE INKEY$ = ""

OPEN "GRID3D.HI" FOR INPUT AS 1
IF errorflag <> 0 THEN
    errorflag = 0
    CLOSE
    GOTO resetscores
END IF

count = 1
DO WHILE NOT EOF(1) AND count <= 5
    LINE INPUT #1, hsfile(count)
    count = count + 1
LOOP

CLOSE 1

IF count < 6 OR hsfile(5) = "" OR hsfile(4) = "" OR hsfile(3) = "" OR hsfile(2) = "" OR hsfile(1) = "" THEN GOTO resetscores

Hscores(1) = VAL(hsfile(1))
Hscores(2) = VAL(hsfile(2))
Hscores(3) = VAL(hsfile(3))
Hscores(4) = VAL(hsfile(4))
Hscores(5) = VAL(hsfile(5))

GOTO done

resetscores:

Hscores(1) = 15000
Hscores(2) = 9000
Hscores(3) = 5000
Hscores(4) = 2000
Hscores(5) = 1000

done:

END SUB

SUB SaveHi

DIM cline AS INTEGER

' *** OS/X HACK: detect if running from .app & change working directory
' *** UNCOMMENT FOR QB64 ***
' Startdir$ = LCASE$(_CWD$)
' PRINT "Current directory", Startdir$
' IF INSTR(Startdir$, ".app") THEN
'    CHDIR "../Resources"
' PRINT "Changed directory to ", _CWD$
' END IF
' PRINT "Saving highscores..."
' DO: LOOP WHILE INKEY$ = ""

OPEN "GRID3D.HI" FOR OUTPUT AS 1
IF errorflag <> 0 THEN
    errorflag = 0
    CLOSE
    GOTO skip
END IF


FOR cline = 1 TO 5
    PRINT #1, STR$(Hscores(cline))
NEXT cline

CLOSE 1

skip:

END SUB

SUB Splash

DIM splscr(24) AS STRING
DIM startline, splashlen AS INTEGER

CLS
COLOR 3
LOCATE 1, 75
IF VidMode = 7 THEN LOCATE 1, 35
PRINT "v1.3"
          
IF vrdevice = 2 AND VidMode <> 7 THEN
    startline = 4
    splscr(1) = "          ÚÄÄÄÄÄ¿ÚÄÄÄÄÄ¿ÚÄÄÄ¿ÚÄÄÄÄÄ¿        ÚÄÄÄÄÄ¿ÚÄÄÄÄÄ¿ÚÄÄÄ¿ÚÄÄÄÄÄ¿"
    splscr(2) = "          ³ ÚÄÄÄÙ³ ÚÄ¿ ³À¿ ÚÙÀ¿ Ú¿ ³        ³ ÚÄÄÄÙ³ ÚÄ¿ ³À¿ ÚÙÀ¿ Ú¿ ³"
    splscr(3) = "          ³ ³ÚÄÄ¿³ ÀÄÙ ³ ³ ³  ³ ³³ ³        ³ ³ÚÄÄ¿³ ÀÄÙ ³ ³ ³  ³ ³³ ³"
    splscr(4) = "          ³ ³À¿ ³³ Ú¿ ÚÙ ³ ³  ³ ³³ ³        ³ ³À¿ ³³ Ú¿ ÚÙ ³ ³  ³ ³³ ³"
    splscr(5) = "          ³ ÀÄÙ ³³ ³³ À¿ÚÙ À¿ÚÙ ÀÙ ³        ³ ÀÄÙ ³³ ³³ À¿ÚÙ À¿ÚÙ ÀÙ ³"
    splscr(6) = "          ÀÄÄÄÄÄÙÀÄÙÀÄÄÙÀÄÄÄÙÀÄÄÄÄÄÙ        ÀÄÄÄÄÄÙÀÄÙÀÄÄÙÀÄÄÄÙÀÄÄÄÄÄÙ"
    splscr(7) = "           þþ  F I G H T E R  3D þþ          þþ F I G H T E R  3D  þþ "
    splscr(8) = "                                                                      "
    splscr(9) = "            You are triangle. Shoot           You are triangle. Shoot "
    splscr(10) = "            squares. Get points!              squares. Get points!    "
    splscr(11) = "                                                                      "
    splscr(12) = "            Controls: Space - fire            Controls: Space - fire  "
    splscr(13) = "             Arrows - move X,Y                 Arrows - move X,Y      "
    splscr(14) = "             A/Z - move in / out               A/Z - move in / out    "
    splscr(15) = "                                                                      "
    splscr(16) = "           þþþ  SPACE TO BEGIN  þþþ          þþþ  SPACE TO BEGIN  þþþ "
    splashlen = 16
ELSEIF vrdevice = 9 OR VidMode = 7 THEN
    startline = 1
    splscr(1) = "    ÚÄÄÄÄÄ¿ÚÄÄÄÄÄ¿ÚÄÄÄ¿ÚÄÄÄÄÄ¿ ÚÂÄÄ¿"
    splscr(2) = "    ³ ÚÄÄÄÙ³ ÚÄ¿ ³À¿ ÚÙÀ¿ Ú¿ ³ ÃÅÄ  "
    splscr(3) = "    ³ ³ÚÄÄ¿³ ÀÄÙ ³ ³ ³  ³ ³³ ³ ÀÙ   "
    splscr(4) = "    ³ ³À¿ ³³ Ú¿ ÚÙ ³ ³  ³ ³³ ³ Ú¿   "
    splscr(5) = "    ³ ÀÄÙ ³³ ³³ À¿ÚÙ À¿ÚÙ ÀÙ ³ Ã´   "
    splscr(6) = "    ÀÄÄÄÄÄÙÀÄÙÀÄÄÙÀÄÄÄÙÀÄÄÄÄÄÙ ÀÙ   "
    splscr(7) = "            = [ 3 D ] =        ÚÂÄÄ "
    splscr(8) = "                               Ã´ Â¿"
    splscr(9) = "    þþþ brought to you by þþþ  ÀÁÄÁÙ"
    splscr(10) = "       DESiRE and PORTA2NOTE   Ú¿  ¿"
    splscr(11) = "                               ÃÅÄÄ´"
    splscr(12) = "     You are triangle. Shoot   ÀÙ  Ù"
    splscr(13) = "     squares. Collect points.  ÚÄÂÂÄ¿"
    splscr(14) = "                                 Ã´  "
    splscr(15) = "     Controls: Space - fire      ÀÙ  "
    splscr(16) = "       Arrows - move X,Y       ÚÂÄÄ¿ "
    splscr(17) = "       A/Z - move in / out     ÃÅÄ   "
    splscr(18) = "                               ÀÁÄÄÙ "
    splscr(19) = "    Code+design: jmph/dSr^p2n  ÚÂÄÄ¿ "
    splscr(20) = "    Add'l code: hellmood/dSr   ÃÅÄÂÙ "
    splscr(21) = "                               ÀÙ À  "
    splscr(22) = "     þþþ SPACEBAR TO BEGIN þþþ       "
    splashlen = 22
ELSE
    startline = 1
    splscr(1) = "    ÚÄÄÄÄÄ¿ÚÄÄÄÄÄ¿ÚÄÄÄ¿ÚÄÄÄÄÄ¿ ÚÂÄÄ¿       ÚÄÄÄÄÄ¿ÚÄÄÄÄÄ¿ÚÄÄÄ¿ÚÄÄÄÄÄ¿ ÚÂÄÄ¿ "
    splscr(2) = "    ³ ÚÄÄÄÙ³ ÚÄ¿ ³À¿ ÚÙÀ¿ Ú¿ ³ ÃÅÄ         ³ ÚÄÄÄÙ³ ÚÄ¿ ³À¿ ÚÙÀ¿ Ú¿ ³ ÃÅÄ   "
    splscr(3) = "    ³ ³ÚÄÄ¿³ ÀÄÙ ³ ³ ³  ³ ³³ ³ ÀÙ          ³ ³ÚÄÄ¿³ ÀÄÙ ³ ³ ³  ³ ³³ ³ ÀÙ    "
    splscr(4) = "    ³ ³À¿ ³³ Ú¿ ÚÙ ³ ³  ³ ³³ ³ Ú¿          ³ ³À¿ ³³ Ú¿ ÚÙ ³ ³  ³ ³³ ³ Ú¿    "
    splscr(5) = "    ³ ÀÄÙ ³³ ³³ À¿ÚÙ À¿ÚÙ ÀÙ ³ Ã´          ³ ÀÄÙ ³³ ³³ À¿ÚÙ À¿ÚÙ ÀÙ ³ Ã´    "
    splscr(6) = "    ÀÄÄÄÄÄÙÀÄÙÀÄÄÙÀÄÄÄÙÀÄÄÄÄÄÙ ÀÙ          ÀÄÄÄÄÄÙÀÄÙÀÄÄÙÀÄÄÄÙÀÄÄÄÄÄÙ ÀÙ    "
    splscr(7) = "            = [ 3 D ] =        ÚÂÄÄ              = [  3  D  ] =       ÚÂÄÄ  "
    splscr(8) = "                               Ã´ Â¿                                  Ã´ Â¿ "
    splscr(9) = "    þþþ brought to you by þþþ  ÀÁÄÁÙ       þþþ brought to you by þþþ  ÀÁÄÁÙ "
    splscr(10) = "       DESiRE and PORTA2NOTE   Ú¿  ¿         DESiRE and PORTA2NOTE    Ú¿  ¿ "
    splscr(11) = "                               ÃÅÄÄ´                                  ÃÅÄÄ´ "
    splscr(12) = "     You are triangle. Shoot   ÀÙ  Ù        You are triangle. Shoot   ÀÙ  Ù "
    splscr(13) = "     squares. Collect points.  ÚÄÂÂÄ¿       squares. Collect points.  ÚÄÂÂÄ¿"
    splscr(14) = "                                 Ã´                                     Ã´  "
    splscr(15) = "     Controls: Space - fire      ÀÙ         Controls: Space - fire      ÀÙ  "
    splscr(16) = "       Arrows - move X,Y       ÚÂÄÄ¿          Arrows - move X,Y       ÚÂÄÄ¿ "
    splscr(17) = "       A/Z - move in / out     ÃÅÄ            A/Z - move  in/out      ÃÅÄ   "
    splscr(18) = "                               ÀÁÄÄÙ                                  ÀÁÄÄÙ "
    splscr(19) = "    Code+design: jmph/dSr^p2n  ÚÂÄÄ¿       Code+design: jmph/dSr^p2n  ÚÂÄÄ¿ "
    splscr(20) = "    Add'l code: hellmood/dSr   ÃÅÄÂÙ       Add'l code: hellmood/dSr   ÃÅÄÂÙ "
    splscr(21) = "                               ÀÙ À                                   ÀÙ À  "
    splscr(22) = "     þþþ SPACEBAR TO BEGIN þþþ             þþþ SPACEBAR TO BEGIN þþþ        "
    splashlen = 22
END IF

DO
    LOCATE startline, 1
    COLOR INT(RND * 7) + 9
    FOR count = 1 TO splashlen
        _LIMIT 10 ' **** UNCOMMENT FOR QB64 ****

        a$ = INKEY$
        LOCATE count + startline, 1
        PRINT splscr(count)

        'eyebleed = TIMER ' **** COMMENT THIS LOOP FOR QB64 ****
        'DO ' **
        '    IF a$ = " " GOTO unsplash
        '    i = TIMER ' **
        'LOOP UNTIL i - eyebleed > .0625 ' ***********************************

        IF a$ = " " GOTO unsplash ' ** UNCOMMENT FOR QB64 JESUS FUCK **

    NEXT count
LOOP WHILE a$ <> " "

unsplash:
                            
END SUB

SUB VRSetup
'SHARED vrdevice

' 1 = Forte VFX-1
' 2 = Oculus DK1
' 3 = other/OSVR/etc.
' 4 = VR64
' 9 = 3D shutter glasses

CLS

LOCATE 3, 1

COLOR 7
PRINT "   Select your VR headset"
PRINT "    1. Forte VFX-1"
PRINT "    2. Oculus DK1 or compatible"
PRINT "    3. OpenHMD/Other (use this option if you don't have a headset)"
PRINT "    4. VR64 or compatible"

DO
    _LIMIT 30 ' **** UNCOMMENT FOR QB64 ****
    ModeGet$ = INKEY$
    IF ModeGet$ = "1" OR ModeGet$ = "2" OR ModeGet$ = "3" OR ModeGet$ = "4" OR ModeGet$ = "5" OR ModeGet$ = "9" THEN vrdevice = VAL(ModeGet$)

    IF ModeGet$ = "1" THEN
        CLS
  
        LOCATE 3, 1
        COLOR 7
        PRINT "   Select your VR headset"
        COLOR 15
        PRINT "    1. Forte VFX-1"
        COLOR 7
        PRINT "    2. Oculus DK1 or compatible"
        PRINT "    3. OpenHMD/Other"
        PRINT "    4. VR64 or compatible"
   
        LOCATE 9, 1
        COLOR 7
        PRINT "   Make sure you run SETMODE.COM before running this game if using a VFX-1."
        PRINT ""
        PRINT "   NOTE: things you REALLY don't want to do include running SETMODE.COM if you"
        PRINT "   don't actually have a VFX-1! Make sure the ISA VIP card is installed, the"
        PRINT "   headset is connected, and the DOS driver (vfx1.com) is loaded before using"
        PRINT "   SETMODE."
        PRINT ""
        PRINT "   Press space to continue or ESC to quit."
    ELSEIF ModeGet$ = "2" THEN
        CLS
 
        LOCATE 3, 1
        COLOR 7
        PRINT "   Select your VR headset"
        PRINT "    1. Forte VFX-1"
        COLOR 15
        PRINT "    2. Oculus DK1 or compatible"
        COLOR 7
        PRINT "    3. OpenHMD/Other"
        PRINT "    4. VR64 or compatible"
  
        LOCATE 9, 1
        COLOR 7
        PRINT "   If using real DOS on your PC, the DK1 should be connected to your video"
        PRINT "   card's DVI port. You don't need the USB cable. Note that the DK1 ONLY"
        PRINT "   supports true DVI-D so a DVI to VGA adapter won't work. Some video cards"
        PRINT "   won't enable the DVI video in DOS if a VGA monitor is also connected (e.g."
        PRINT "   Nvidia Quadro 2 / Geforce 2 GTS) so try unplugging any other monitors and"
        PRINT "   running the DK1 exclusively if you don't get a display on the headset."
        PRINT ""
        PRINT "   For DOSBox, set the native resolution to 1280x800 in dosbox.conf, enable"
        PRINT "   full-screen scaling, and disable aspect-ratio correction. The DK1 must be"
        PRINT "   running in extended desktop mode (not direct-to-Rift.) You might want to"
        PRINT "   mirror the main display & the DK1"
        PRINT ""
        PRINT "   Press space to continue or ESC to quit."
    ELSEIF ModeGet$ = "3" THEN
        CLS

        LOCATE 3, 1
        COLOR 7
        PRINT "   Select your VR headset"
        PRINT "    1. Forte VFX-1"
        PRINT "    2. Oculus DK1 or compatible"
        COLOR 15
        PRINT "    3. OpenHMD/Other"
        COLOR 7
        PRINT "    4. VR64 or compatible"
 
        LOCATE 9, 1
        COLOR 7
        PRINT "   This is a generic profile that should be compatible with many devices."
        PRINT ""
        PRINT "   To change the stereo geometry to work with your headset, use the following"
        PRINT "   keys in-game:   ,.  -  zoom playfield"
        PRINT "                   ()  -  stereo balance"
        PRINT "                   []  -  eye spacing"
        PRINT "   If the next spash screen displays correctly, the default parameters will"
        PRINT "   work and nothing needs to be changed."
        PRINT ""
        PRINT "   You can also use this option if you don't have a headset."
        PRINT ""
        PRINT "   Press space to continue or ESC to quit."
    ELSEIF ModeGet$ = "4" THEN
        CLS

        LOCATE 3, 1
        COLOR 7
        PRINT "   Select your VR headset"
        PRINT "    1. Forte VFX-1"
        PRINT "    2. Oculus DK1 or compatible"
        PRINT "    3. OpenHMD/Other"
        COLOR 15
        PRINT "    4. VR64 or compatible"

        LOCATE 9, 1
        COLOR 7
        PRINT "   This profile has been created with a home-built VR64 clone using the same"
        PRINT "   generic headset and a VGA panel similar in size to the original. You may"
        PRINT "   need to adjust the geometry to suit your version of this device, especially"
        PRINT "   if you're using composite video."
        PRINT ""
        PRINT "   To change the stereo geometry to work with your headset, use the following"
        PRINT "   keys in-game:   ,.  -  zoom playfield"
        PRINT "                   ()  -  stereo balance"
        PRINT "                   []  -  eye spacing"
        PRINT "   If the next spash screen displays correctly, the default parameters will"
        PRINT "   work and nothing needs to be changed."
        PRINT ""
        PRINT "   Press space to continue or ESC to quit."
    ELSEIF ModeGet$ = "5" THEN
        CLS

        LOCATE 3, 1
        COLOR 7
        PRINT "   Select your VR headset"
        PRINT "    1. Forte VFX-1"
        PRINT "    2. Oculus DK1 or compatible"
        PRINT "    3. OpenHMD/Other"
        PRINT "    4. VR64 or compatible"
        COLOR 15
        PRINT "    5. HTC Vive                                                "

        LOCATE 10, 1
        COLOR 7
        PRINT "   Oh yes. We're going there."
        PRINT ""
        PRINT ""
        PRINT "   Unfortunately, we're not there *yet*, and this mode isn't finished."
        PRINT ""
        PRINT "   WANTED: HTC Vive. Cheap. Contact us."
    ELSEIF ModeGet$ = "9" THEN
        CLS

        LOCATE 3, 1
        COLOR 7
        PRINT "   Select your VR headset"
        PRINT "    1. Forte VFX-1"
        PRINT "    2. Oculus DK1 or compatible"
        PRINT "    3. OpenHMD/Other"
        PRINT "    4. VR64 or compatible"

        COLOR 15
        PRINT "    9. SECRET PIRATE MODE (double-lowres, booyah!)               "
        COLOR 7

        LOCATE 10, 1
        COLOR 7
        PRINT "   This mode is being added for future support of 3D shutter glasses using"
        PRINT "   Donald Sawdai's LCDBIOS. It's not finished yet either."
        PRINT ""
        PRINT "   For now, you can use this mode to play without a stereo image pair. This"
        PRINT "   will also be faster on REALLY low-end machines (286??) without a headset."
        PRINT ""
        PRINT "   It's not RECOMMENDED to play this way, but if you really want to, be our"
        PRINT "   guest."
        PRINT ""
        PRINT "   Press space to continue or ESC to quit."
    ELSEIF ModeGet$ = CHR$(27) THEN
        vrdevice = 99
    END IF
LOOP UNTIL ModeGet$ = " " OR ModeGet$ = CHR$(27)

IF vrdevice = 0 THEN vrdevice = 3
IF vrdevice = 4 THEN vrdevice = 3 ' Same parameters work, being lazy
IF vrdevice = 5 THEN vrdevice = 3 ' Vive support isn't finished, use generic profile

' NOTE: there are probably bugs related to different values of vrdevice

END SUB

