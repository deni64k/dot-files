-- Haskell common
import qualified Data.Map as M
import System.IO(hPutStrLn)
import System.IO.Unsafe

import System.Exit

import System.Directory

-- XMonad common
import XMonad
import qualified XMonad.StackSet as W

-- XMonad actions
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap -- not used currently
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.Search as Search

-- XMonad hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

-- XMonad layouts
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

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
-- backgroundColor = "#2c2c32"
-- foregroundColor = "grey70"

regularFont = "-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-r"
xftFont = "Monaco:pixelsize=10"

statusBarCmd = "dzen2"
               ++ " -bg '" ++ backgroundColor ++ "'"
               ++ " -fg '" ++ foregroundColor ++ "'"
               ++ " -fn '" ++ xftFont ++ "'"
               ++ " -sa c -ta l -w 1250 -h 16"

homeDirectory = unsafePerformIO System.Directory.getHomeDirectory

-- XPConfig options:
ownXPConfig = defaultXPConfig
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
                  , startupHook = return () >> checkKeymap defaultConfig (keys' defaultConfig) >> ewmhDesktopsStartup >> setWMName "LG3D"
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

--
-- mod1Mask = alt
-- mod4Mask = windows key
-- mod5Mask = hyper key
--
modMask' :: KeyMask
modMask' = mod4Mask

workspaces' :: [WorkspaceId]
workspaces' = ["emacs", "two", "three", "four", "five", "net", "fun", "listen", "console"]

borderWidth' :: Dimension
borderWidth' = 2

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = backgroundColor
focusedBorderColor' = colorDodgerBlue -- "#adff2f"

terminal' :: String
terminal' = "urxvt"

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
          , ("M-p"         , shellPrompt ownXPConfig)
          , ("M-S-p"       , prompt (terminal' ++ " -e") ownXPConfig)
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
          -- switch layout
          , ("M-/"         , sendMessage NextLayout)
          -- XMonad general
          , ("M-q"         , restart "xmonad" True)
          , ("M-S-q"       , io (exitWith ExitSuccess))
          -- switch workspace
          , ("M-M1-z"      , prevWS)
          , ("M-M1-x"      , nextWS)
          , ("M-t e"       , Search.promptSearch ownXPConfig lingvoEnRu)
          , ("M-t r"       , Search.promptSearch ownXPConfig lingvoRuEn)
          -- some gap-toggling
          , ("M-C-p b", sendMessage $ ToggleStrut D)
          , ("M-C-p t", sendMessage $ ToggleStrut U)
          , ("M-C-p f", sendMessage $ ToggleStrut L)
          , ("M-C-p h", sendMessage $ ToggleStrut R)
          , ("M-C-p a", sendMessage $ ToggleStruts)

          , ("M-C-p g", sendMessage $ ToggleGaps)
          -- VPN connect/disconnect
          , ("M-C-n c", unsafeSpawn "sudo vpnc ~/.vpn-acronis")
          , ("M-C-n d", unsafeSpawn "sudo vpnc-disconnect")
          , ("M-C-n r", unsafeSpawn "remmina")
          ] ++
          -- mod-[1..9], Switch to workspace N
          -- mod-shift-[1..9], Move client to workspace N
          [ (m ++ k, windows $ f w)
                | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
                , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)] ]
          -- ++
          -- -- Search methods
          -- -- mapped to mod-c for 'a căuta'
          -- -- FIXME: broken after recent 'safeSpawn' String -> [String] changes
          -- [("M-c " ++ k, Search.promptSearch largeXPConfig f) | (k,f) <- searchList ]
          -- ++
          -- [("M-C-c " ++ k, Search.selectSearch f) | (k,f) <- searchList ]
          --     where -- | non-empty workspaces less scratchpad
          --       shiftAndView dir = findWorkspace getSortByIndexNoSP dir NonEmptyWS 1
          --                          >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
          --       getSortByIndexNoSP =
          --           fmap (.scratchpadFilterOutWorkspace) getSortByIndex

-- logHook --------------------------------------------------------------------

-- dynamicLog pretty printer for dzen
pp' din = defaultPP
            { ppCurrent = wrap "^bg(#262626)" "^bg()"
                          . wrap leftBracket rightBracket
                          . wrap "^fg(#ffa500)" "^fg()"
            , ppVisible = wrap "^bg(grey30)^fg(grey75)" "^fg()^bg()"
            , ppSep     = " ^p(2)^fg(grey60)^r(3x3)^fg() "
            , ppWsSep   = " "
            , ppLayout  = dzenColor colorDodgerBlue ""
                          . (\x -> case x of
                                    "Mirror ResizableTall"                   -> pad $ xbm "mtall"
                                    "ResizableTall"                          -> pad $ xbm "tall"
                                    "Full"                                   -> pad $ xbm "full"
                                    "Magnifier GridRatio 1.3333333333333333" -> pad $ xbm "mgrid"
                                    "GridRatio 1.3333333333333333"           -> pad $ xbm "grid"
                                    "ReflectX Gimp"                          -> pad $ xbm "reflectx"
                                    _                                        -> pad x)
            , ppTitle   = dzenColor foregroundColor "" . wrap leftBracket rightBracket
            , ppOutput  = hPutStrLn din
            }
          where
            leftBracket  = "^fg(" ++ colorDodgerBlue ++ ")[^fg()"
            rightBracket = "^fg(" ++ colorDodgerBlue ++ ")]^fg()"
            xbm fname = "^i(" ++ xbmDirectory ++ "/" ++ fname ++ ".xbm)"
            xbmDirectory = homeDirectory ++ "/.xmonad/dzen"

logHook' din = (dynamicLogWithPP $ pp' din)
               >> updatePointer (Relative 0.5 0.5)

-- tabConfig' -----------------------------------------------------------------

tabConfig' = defaultTheme
  { activeColor   = "#555753"
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

-- layoutHook' ----------------------------------------------------------------

layoutHook' = avoidStrutsOn [U, D]
            $ gaps (zip [U, D, L, R] (repeat 0))
            $ smartBorders
            $ onWorkspaces ["emacs", "net", "console"]
                           (Full ||| tiled' ||| (Mirror tiled'))
            $ tiled' ||| (Mirror tiled') ||| Full
    where tiled' = ResizableTall 1 (2/100) (1/2) []

-- manageHook -----------------------------------------------------------------

manageHook' = composeAll
  -- force floating
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , className =? "Gajim.py"       --> doFloat
  , className =? "Downbar"        --> doFloat
  , className =? "Extension"      --> doFloat
  , className =? "Qutim"          --> doFloat
  , className =? "Skype"          --> doFloat
  , appName   =? "Download"       --> doFloat
  , appName   =? "Dialog"         --> doFloat
  , appName   =? "xmessage"       --> doFloat
  , appName   =? "QEMU"           --> doFloat

  -- bind windows to workspaces
  , className =? "Chromium"       --> doF (W.shift "net")
  , className =? "Thunderbird"    --> doF (W.shift "net")
  , className =? "Icedove"        --> doF (W.shift "net")
  , className =? "Firefox"        --> doF (W.shift "net")
  , className =? "Iceweasel"      --> doF (W.shift "net")
  , className =? "Gajim.py"       --> doF (W.shift "fun")
  , className =? "Sonata"         --> doF (W.shift "listen")
  , className =? "xconsole"       --> doF (W.shift "console")
  , className =? "XConsole"       --> doF (W.shift "console")

  , className =? "stalonetray"    --> doIgnore
  , className =? "trayer"         --> doIgnore
  , className =? ""               --> doIgnore
  ]

-- mouseBindings --------------------------------------------------------------

mouseBindings' (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
  ]

-- searchList -----------------------------------------------------------------
lingvoEnRu = Search.searchEngine "lingvo (en->ru)" "http://lingvo.yandex.ru/en?lang=en&search_type=lingvo&st_translate=1&text="
lingvoRuEn = Search.searchEngine "lingvo (ru->en)" "http://lingvo.yandex.ru/en?lang=ru&search_type=lingvo&st_translate=1&text="

searchList :: [([Char], Search.SearchEngine)]
searchList = [ ("g", Search.google)
             , ("l", lingvoEnRu)
             -- , ("i", Search.isohunt)
             , ("w", Search.wikipedia)
             , ("d", Search.dictionary)
             , ("t", Search.thesaurus)
             , ("a", Search.amazon)
             , ("y", Search.youtube)
             -- , ("e", enro40)
             -- , ("r", roen40)
             ]
