-- Haskell common
import qualified Data.Map as M
import System.IO.UTF8(hPutStrLn)

import System.Exit

-- XMonad common
import XMonad
import qualified XMonad.StackSet as W

-- XMonad actions
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap -- not used currently
import XMonad.Actions.CycleWS

-- XMonad hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- XMonad layouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders(smartBorders, noBorders)
import XMonad.Layout.Circle
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders

-- XMonad utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Actions
import XMonad.Actions.WindowGo (runOrRaise)

-- general --------------------------------------------------------------------

backgroundColor = "#ecedee"

statusBarCmd = "dzen2 -sa c -ta l -bg '" ++ backgroundColor ++ "' -fg '#000000' -w 1600 -fn '-*-terminus-medium-r-normal-*-12-*-*-*-*-*-iso10646-1' -h 16"

-- XPConfig options:
myXPConfig = defaultXPConfig
    { font              = "-xos4-terminus-medium-r-normal-*-12-*-*-*-c-*-iso10646-1"
    , bgColor           = backgroundColor
    , fgColor           = "#000000"
    , fgHLight          = "#ffffff"
    , bgHLight          = "#555753"
    , borderColor       = "#ede9e3"
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 16
    , historySize       = 100
    }

-- main -----------------------------------------------------------------------

main = do
       din <- spawnPipe statusBarCmd
       xmonad $ defaultConfig
                  { workspaces = workspaces'
                  , modMask = modMask'
                  , borderWidth = borderWidth'
                  , normalBorderColor = normalBorderColor'
                  , focusedBorderColor = focusedBorderColor'
--                  , defaultGaps = defaultGaps'
                  , terminal = terminal'
                  , keys = keys'
                  , logHook = logHook' din
                  , layoutHook = layoutHook'
                  , manageHook = manageHook'
                  , mouseBindings = mouseBindings'
                  }

-- misc -----------------------------------------------------------------------

workspaces' :: [WorkspaceId]
workspaces' = ["one", "two", "three", "mail", "www", "p2p", "chat", "music", "xconsole"]

modMask' :: KeyMask
--
-- mod1Mask = alt
-- mod4Mask = windows key
-- mod5Mask = hyper key
--
modMask' = mod4Mask

borderWidth' :: Dimension
borderWidth' = 2

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#cccccc"
focusedBorderColor' = "#20a020"

defaultGaps' :: [(Int,Int,Int,Int)]
defaultGaps' = [(16,0,0,0)]

terminal' :: String
terminal' = "xterm"

-- multimedia keys ------------------------------------------------------------
xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioMute        = 0x1008ff12
xK_XF86AudioNext        = 0x1008ff17
xK_XF86AudioPrev        = 0x1008ff16
xK_XF86AudioPlay        = 0x1008ff14
xK_XF86AudioStop        = 0x1008ff15
xK_XF86AudioMedia       = 0x1008ff32

-- keys -----------------------------------------------------------------------
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask .|. shiftMask, xK_Return), spawn $ terminal conf)
  -- keys for Window Navigation hook
  , ((modMask,               xK_Right       ), sendMessage $ Go R)
  , ((modMask,               xK_Left        ), sendMessage $ Go L)
  , ((modMask,               xK_Up          ), sendMessage $ Go U)
  , ((modMask,               xK_Down        ), sendMessage $ Go D)
  , ((modMask .|. shiftMask, xK_Right       ), sendMessage $ Swap R)
  , ((modMask .|. shiftMask, xK_Left        ), sendMessage $ Swap L)
  , ((modMask .|. shiftMask, xK_Up          ), sendMessage $ Swap U)
  , ((modMask .|. shiftMask, xK_Down        ), sendMessage $ Swap D)

  -- run programs
  , ((modMask,               xK_p           ), shellPrompt myXPConfig)
  , ((modMask .|. shiftMask, xK_p           ), prompt (terminal' ++ " -e") myXPConfig)

  --
  , ((modMask,               xK_KP_Multiply ), spawn "mymixer --add 3")
  , ((modMask,               xK_KP_Divide   ), spawn "mymixer --sub 3")

  -- multimedia keys
  , ((0,            xK_XF86AudioLowerVolume ), spawn "mpc volume -2")
  , ((0,            xK_XF86AudioRaiseVolume ), spawn "mpc volume +2")
  , ((0,            xK_XF86AudioMute        ), spawn "mymixer --mute")
  , ((0,            xK_XF86AudioNext        ), spawn "mpc next")
  , ((0,            xK_XF86AudioPrev        ), spawn "mpc prev")
  , ((0,            xK_XF86AudioPlay        ), spawn "mpc toggle")
  , ((0,            xK_XF86AudioStop        ), spawn "mpc stop")
  , ((0,            xK_XF86AudioMedia       ), runOrRaise "sonata" (className =? "Sonata"))

  -- adjust current window
  , ((modMask,               xK_f           ), withFocused $ windows . W.sink)
  , ((modMask,               xK_c           ), kill)

  -- switch window
  , ((modMask,               xK_j           ), windows W.focusUp)
  , ((modMask,               xK_k           ), windows W.focusDown)

  -- resizing
  , ((modMask,               xK_h           ), sendMessage Shrink)
  , ((modMask,               xK_l           ), sendMessage Expand)
  , ((modMask .|. shiftMask, xK_h           ), sendMessage MirrorShrink)
  , ((modMask .|. shiftMask, xK_l           ), sendMessage MirrorExpand)

  -- adjust current layout
  , ((modMask .|. mod1Mask,  xK_comma       ), sendMessage (IncMasterN (-1)))
  , ((modMask .|. mod1Mask,  xK_period      ), sendMessage (IncMasterN 1))

  -- toggle the status bar gap
  , ((modMask,               xK_b           ), sendMessage ToggleStruts)

  -- switch layout
  , ((modMask,               xK_slash       ), sendMessage NextLayout)

  -- switch workspace
  , ((modMask .|. mod1Mask,  xK_z           ), prevWS)
  , ((modMask .|. mod1Mask,  xK_x           ), nextWS)

  -- XMonad general
  , ((modMask,               xK_q           ), restart "/usr/home/dennis/.cabal/bin/xmonad" True)
  , ((modMask .|. shiftMask, xK_q           ), io (exitWith ExitSuccess))
  ]
  ++

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]]

-- logHook --------------------------------------------------------------------

-- dynamicLog pretty printer for dzen
pp' din = defaultPP
            { ppCurrent         = wrap "^bg(#555753)^fg(#ffffff) " " ^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
            , ppVisible         = wrap " " " "                                   . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
            , ppHidden          = wrap " " " "                                   . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
            , ppHiddenNoWindows = wrap " " " "                                   . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
            , ppSep             = " :: "
            , ppWsSep           = ""
            , ppLayout          = dzenColor "" ""
            , ppTitle           = dzenColor "" ""
            , ppOutput          = hPutStrLn din
            }

logHook' din = (dynamicLogWithPP $ pp' din)
               >> updatePointer (Relative 0.5 0.5)

-- layoutHook -----------------------------------------------------------------

tabConfig' = defaultTheme
  {
    activeColor   = "#555753"
  , inactiveColor = "#ede9e3"
  , urgentColor   = "#ede9e3"

  , activeBorderColor   = "#555753"
  , inactiveBorderColor = "#ede9e3"
  , urgentBorderColor   = "#ede9e3"

  , activeTextColor   = "#ffffff"
  , inactiveTextColor = "#000000"
  , urgentTextColor   = "#000000"

  , fontName = "-*-terminus-medium-r-normal-*-12-*-*-*-*-*-iso10646-1"
  }

--layoutHook' = configurableNavigation noNavigateBorders $ smartBorders $ layouts
--layoutHook' = smartBorders $ layouts
layoutHook' = layouts

-- layouts
layouts = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full
  where
    tiled = ResizableTall 1 (2/100) (1/2) []
{-
layouts = avoidStruts
        $ onWorkspace "mail"  myTabbed
        $ onWorkspace "www"   myTabbed
        $ onWorkspace "music" myTabbed
        $ tiled ||| myTabbed ||| noBorders Full ||| Circle
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
    myTabbed = noBorders(tabbed shrinkText tabConfig')
-}
-- manageHook -----------------------------------------------------------------
-- http://www.nntt.org/download.php?id=179
manageHook' = composeAll
  [
  -- force floating
    className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , className =? "Gajim.py"       --> doFloat
  , appName   =? "Download"       --> doFloat
  , appName   =? "Dialog"         --> doFloat

  -- bind windows to workspaces
  , className =? "Thunderbird"    --> doF (W.shift "mail")
  , className =? "Icedove"        --> doF (W.shift "mail")
  , className =? "Firefox"        --> doF (W.shift "www")
  , className =? "Iceweasel"      --> doF (W.shift "www")
  , className =? "Gajim.py"       --> doF (W.shift "chat")
  , className =? "Sonata"         --> doF (W.shift "music")
  , className =? "xconsole"       --> doF (W.shift "xconsole")
  , className =? "XConsole"       --> doF (W.shift "xconsole")

  , className =? "stalonetray"    --> doIgnore
  , className =? "trayer"         --> doIgnore
  , className =? ""               --> doIgnore
  ]

-- mouseBindings --------------------------------------------------------------

mouseBindings' (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
  ]
