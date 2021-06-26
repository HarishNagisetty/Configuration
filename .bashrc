# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

############
# Settings #
############

export EDITOR="vim"

###########
# Aliases #
###########

alias ls='ls --color=auto -F'
alias l=ls
alias ll="ls -o"
alias la="ls -a"

alias ..="cd ..; ls"
alias ....="cd ../..; ls"
alias ......="cd ../../..; ls"

alias hn-copy-pwd="pwd | tr -d '\n' | xclip; pwd"

alias emc="emacsclient -n"

#############
# Functions #
#############

cl () { cd $1; ls; }

hn-gen-password () {
    tr -dc A-Za-z0-9 < /dev/urandom | head -c $1 ; echo ''
}

# Inactive {{{
#hn-extract-archive () {
#    if [ -f $1 ] ; then
#        case $1 in
#            *.tar.*)     tar xvaf $1    ;;
#            *.bz2)       bunzip2 $1     ;;
#            *.rar)       unrar x $1       ;;
#            *.gz)        gunzip $1      ;;
#            *.tar)       tar xvf $1     ;;
#            *.tbz2)      tar xvjf $1    ;;
#            *.tgz)       tar xvzf $1    ;;
#            *.zip)       unzip $1       ;;
#            *.Z)         uncompress $1  ;;
#            *.7z)        7z x $1        ;;
#            *)           echo "don't know how to extract '$1'..." ;;
#        esac
#    else
#        echo "'$1' is not a valid file!"
#    fi
#}

#hn-up () {
#    cd $(eval printf '../'%.0s {1..$1}) && pwd
#}

#hn-du () {
#    if [[ $@ ]]; then
#        du -sh $@ | sort -h
#    else
#        du -sh * | sort -h
#    fi
#}
# }}}

###########
# Private #
###########

if [ -f ~/.bashrc.private ]; then
    source ~/.bashrc.private
fi

