import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)

import System.IO (hPutStrLn)

term = "urxvt"

-- Dzen colours and height settings
dzenStyle = " -h 12  -y '0' -fg '#777777' -bg '#222222'"

-- Dimensions of the xmonad bar
xmonadStatus = "dzen2 -x '0' -w '400' -ta 'l'" ++ dzenStyle

-- Dimensions of the "widget" bar
widgetBar = "dzen_widgets.sh | dzen2 -x '400' -w '880' -ta 'r'" ++ dzenStyle

-- Get information from xmoand and give it to dzen
loggingHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h}

-- How the text given from xmonad looks in the dzen bar
myDzenPP = dzenPP
    {
       ppCurrent = dzenColor "#3399ff" "" . wrap " " " ",
       ppHidden = dzenColor "#dddddd" "" . wrap " " " ",
       ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " ",
       ppUrgent = dzenColor "#ff0000" "" . wrap " " " ",
       ppSep = " | ",
       ppLayout = \y -> "",
       ppTitle = dzenColor "#ffffff" "" . wrap " " " "
    }

-- Layout hook
myLayoutHook = avoidStruts (tall)
                where tall = Tall 1 (3/100) (1/2)

-- Workspace titles
myWorkspaces = ["1:main", "2:web", "3", "4", "5", "6", "7", "8", "9"]

main = do

        status  <- spawnPipe xmonadStatus
        widgets <- spawnPipe widgetBar

        xmonad defaultConfig
              { terminal = term,
                handleEventHook = docksEventHook,
                logHook = loggingHook status,
                layoutHook = myLayoutHook,
                manageHook = manageDocks,
                workspaces = myWorkspaces}
