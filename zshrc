# Basic zsh configuration 

# Path to oh-my-zsh configuration
ZSH=$HOME/.oh-my-zsh

# Set the prompt theme
ZSH_THEME="minimal"

# Display dots when waiting on completion
COMPLETION_WAITING_DOTS="true"

# Load oh-my-zsh plugins
plugins=(git)

# Load oh-my-zsh configuration
source $ZSH/oh-my-zsh.sh

# Load syntax highlighting rules
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh  

# Some setting for syntax highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main pattern root brackets cursor)
ZSH_HIGHLIGHT_STYLES[path]='bold'

# Load my list of aliases 
source $HOME/.zsh/aliases 

# Setting the timezone 
TZ='Europe/London' ; export TZ

# Set a number of environment variables 
export EDITOR='vim'
export PATH=$PATH:$HOME/bin
export LD_LIBRARY_PATH="/opt/java/jre/lib/amd64"
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ts=8 nomod nolist nonu noma fdm=indent' -\""

autoload -U colors && colors

# {{{ Functions 

cdl () {  # Clears the screen and list the contents of the directory just entered into 
	
	if [[ $1 = '' ]]; then
		cd ~ && clear && lst
	else 
		cd "$1" && clear && lst 
	fi 

}



function precmd()  # Prints an extra newline between commands which i think looks nicer
{ 
	echo
}


