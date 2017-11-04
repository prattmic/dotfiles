import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.PhysicalScreens
import qualified Data.Map as M
import System.IO
import Graphics.X11.ExtraTypes

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((0,              xK_Print),                  spawn "scrot")                          -- PrintScreen takes screenshot
    , ((controlMask,    xK_Print),                  spawn "sleep 0.2; scrot -s")            -- ^PrintScreen takes screenshot of window
    , ((0,              xF86XK_AudioRaiseVolume),   spawn "pactl set-sink-volume 0 +5%")    -- Volume up
    , ((0,              xF86XK_AudioLowerVolume),   spawn "pactl set-sink-volume 0 -5%") -- Volume down
    , ((0,              xF86XK_AudioMute),          spawn "~/dotfiles/scripts/toggle-mute sink") -- Volume mute
    , ((0,              xF86XK_Launch1),            spawn "urxvt")                          -- ThinkVantage button launches terminal
    , ((0,              xF86XK_Launch2),            spawn "~/dotfiles/scripts/toggle-mute source") -- Mic mute
    , ((modm,           xK_f),                      focusUrgent)                            -- Focus urgent window
    , ((modm,           xK_p),                      spawn "exe=`echo $PATH | sed 's!:! !g' | xargs lsx | yeganesh` && eval \"exec $exe\"")  -- Launch dmenu via yeganesh.  This orders by popularity.
    , ((modm,           xK_b),                      sendMessage ToggleStruts)               -- "Toggle struts"
    ] ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook
           $ docks $ ewmh defaultConfig
        { modMask       = mod4Mask                                     -- Rebind Mod to super key
        , manageHook    = manageDocks <+> manageHook defaultConfig     -- Add support for status bar and dock
        , layoutHook    = avoidStruts  $  layoutHook defaultConfig     -- Add support for status bar and dock
        , startupHook   = setWMName "LG3D"                             -- Set window manager name so that Matlab will open
        , keys          = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook       = dynamicLogWithPP xmobarPP
            { ppOutput  = hPutStrLn xmproc
            , ppTitle   = xmobarColor "green" "" . shorten 100
            , ppUrgent  = xmobarColor "red" ""
            }
        }
