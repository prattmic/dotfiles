import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig     -- Add support for status bar and dock
        , layoutHook = avoidStruts  $  layoutHook defaultConfig     -- Add support for status bar and dock
        , logHook    = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        , startupHook= setWMName "LG3D"                             -- Set window manager name so that Matlab will open
        , modMask    = mod4Mask                                     -- Rebind Mod to super key
        } `additionalKeys`
        [ ((0, xK_Print), spawn "scrot")                            -- PrintScreen takes screenshot
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")    -- ^PrintScreen takes screenshot of window
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")   -- Volume up
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -- -5%")   -- Volume down
        , ((0, xF86XK_AudioMute), spawn "~/scripts/toggle-mute")    -- Volume mute
        ]
