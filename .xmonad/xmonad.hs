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
import XMonad.Util.EZConfig (mkKeymap, checkKeymap, additionalKeys, additionalKeysP)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Actions
import XMonad.Actions.WindowGo (runOrRaise)

-- general --------------------------------------------------------------------

colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorDodgerBlue      = "#1e90ff"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
colorGray5           = "#0d0d0d"
colorGray95          = "#f2f2f2"

backgroundColor = colorGray5
foregroundColor = colorGray95

regularFont = "-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-r"
xftFont = "terminus:size=8"

statusBarCmd = "~/src/dzen/dzen2"
               ++ " -bg '" ++ backgroundColor ++ "'"
               ++ " -fg '" ++ foregroundColor ++ "'"
               ++ " -fn '" ++ xftFont ++ "'"
               ++ " -sa c -ta l -w 1600 -h 16"

-- XPConfig options:
myXPConfig = defaultXPConfig
    { font              = regularFont
    , bgColor           = backgroundColor
    , fgColor           = foregroundColor
    , fgHLight          = "#ffa500"
    , bgHLight          = backgroundColor
    , promptBorderWidth = 0
    , position          = Bottom
    , height            = 16
    , historySize       = 100
    }

-- main -----------------------------------------------------------------------

main = do
       xmobar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
       din <- spawnPipe statusBarCmd
       xmonad $ defaultConfig
                  { workspaces = workspaces'
                  , modMask = modMask'
                  , borderWidth = borderWidth'
                  , normalBorderColor = normalBorderColor'
                  , focusedBorderColor = focusedBorderColor'
                  , terminal = terminal'
                  , keys = \c -> mkKeymap c (keys' c)
                  , logHook = do logHook' din
                                 dynamicLogWithPP defaultPP { ppOutput = \e -> hPutStrLn xmobar "" }
                  , layoutHook = layoutHook'
                  , manageHook = manageHook'
                  , mouseBindings = mouseBindings'
                  , startupHook = return () >> checkKeymap defaultConfig (keys' defaultConfig)
                  }
                  `additionalKeys`
                  -- multimedia keys
                  [ ((0, xK_XF86AudioLowerVolume), unsafeSpawn "mpc volume -2")
                  , ((0, xK_XF86AudioRaiseVolume), unsafeSpawn "mpc volume +2")
                  , ((0, xK_XF86AudioMute       ), unsafeSpawn "mymixer --mute")
                  , ((0, xK_XF86AudioNext       ), unsafeSpawn "mpc next")
                  , ((0, xK_XF86AudioPrev       ), unsafeSpawn "mpc prev")
                  , ((0, xK_XF86AudioPlay       ), unsafeSpawn "mpc toggle")
                  , ((0, xK_XF86AudioStop       ), unsafeSpawn "mpc stop")
                  , ((0, xK_XF86AudioMedia      ), runOrRaise "sonata" (className =? "Sonata"))
                  -- make screenshot
                  , ((0, xK_Print), unsafeSpawn "import -window root $HOME/.tmp/xwd-$(date +%s)$$.png") ]

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
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = backgroundColor
focusedBorderColor' = "#adff2f"

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
keys' c = [ ("M-S-<Return>", spawn $ XMonad.terminal c)
          -- run programs
          , ("M-p"         , shellPrompt myXPConfig)
          , ("M-S-p"       , prompt (terminal' ++ " -e") myXPConfig)
          -- adjust current window
          , ("M-f"         , withFocused $ windows . W.sink)
          , ("M-c"         , kill)
          -- switch window
          , ("M-j"         , windows W.focusUp)
          , ("M-k"         , windows W.focusDown)
          -- resizing
          , ("M-h"         , sendMessage Shrink)
          , ("M-l"         , sendMessage Expand)
          , ("M-S-h"       , sendMessage MirrorShrink)
          , ("M-S-l"       , sendMessage MirrorExpand)
          -- adjust current layout
          , ("M-M1-,"      , sendMessage (IncMasterN (-1)))
          , ("M-M1-."      , sendMessage (IncMasterN 1))
          -- toggle the status bar gap
          , ("M-b"         , sendMessage ToggleStruts)
          -- switch layout
          , ("M-/"         , sendMessage NextLayout)
          -- XMonad general
          , ("M-q"         , restart "xmonad" True)
          , ("M-S-q"       , io (exitWith ExitSuccess))
          -- switch workspace
          , ("M-M1-z"      , prevWS)
          , ("M-M1-x"      , nextWS)
          ] ++
          -- mod-[1..9], Switch to workspace N
          -- mod-shift-[1..9], Move client to workspace N
          [ (m ++ k, windows $ f w)
                | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
                , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)] ]
{-
  , ((modMask,               xK_Right       ), sendMessage $ Go R)
  , ((modMask,               xK_Left        ), sendMessage $ Go L)
  , ((modMask,               xK_Up          ), sendMessage $ Go U)
  , ((modMask,               xK_Down        ), sendMessage $ Go D)
  , ((modMask .|. shiftMask, xK_Right       ), sendMessage $ Swap R)
  , ((modMask .|. shiftMask, xK_Left        ), sendMessage $ Swap L)
  , ((modMask .|. shiftMask, xK_Up          ), sendMessage $ Swap U)
  , ((modMask .|. shiftMask, xK_Down        ), sendMessage $ Swap D)
-}
-- logHook --------------------------------------------------------------------

-- dynamicLog pretty printer for dzen
pp' din = defaultPP
            { ppCurrent         = wrap "^bg(#262626)^fg(#1e90ff)[^fg(#ffa500)" "^fg(#1e90ff)]^fg()^bg()"
            , ppVisible         = wrap "" ""
            , ppHidden          = wrap "" ""
            , ppHiddenNoWindows = wrap "" ""
            , ppSep             = " ^fg(grey60)^r(1x8)^fg() "
            , ppWsSep           = " "
            , ppLayout          = dzenColor colorDodgerBlue ""
                                  . (\x -> case x of
                                            "Mirror ResizableTall"                      -> pad "^i(/home/dennis/.xmonad/dzen/mtall.xbm)"
                                            "ResizableTall"                             -> pad "^i(/home/dennis/.xmonad/dzen/tall.xbm)"
                                            "Full"                                      -> pad "^i(/home/dennis/.xmonad/dzen/full.xbm)"
                                            "Magnifier GridRatio 1.3333333333333333"    -> pad "^i(/home/dennis/.xmonad/dzen/mgrid.xbm)"
                                            "GridRatio 1.3333333333333333"              -> pad "^i(/home/dennis/.xmonad/dzen/grid.xbm)"
                                            "ReflectX Gimp"                             -> pad "^i(/home/dennis/.xmonad/dzen/reflectx.xbm)"
                                            _                                           -> pad x
                                    )
            , ppTitle           = dzenColor foregroundColor ""
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

  , fontName = "-*-terminus--r-*-*-12-*-*-*-*-*-*-r"
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
manageHook' = composeAll
  [
  -- force floating
    className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , className =? "Gajim.py"       --> doFloat
  , appName   =? "Download"       --> doFloat
  , appName   =? "Dialog"         --> doFloat
  , appName   =? "xmessage"       --> doFloat

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
