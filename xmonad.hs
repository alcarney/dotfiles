import XMonad

term = "urxvt -e 'tmux'"

main = xmonad defaultConfig
              { terminal = term }
