import XMonad

term = "urxvt -e zsh -c 'tmuxutils --new-session'"

main = xmonad defaultConfig
              { terminal = term }
