#! /usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}

## Usage: link_file SRC DST
link_file () {
    local source=$1
    local destination=$2

    if [[ ! -e "$source" ]]; then
        echo "$source is not present!"
        return 1
    fi

    if [[ -e "$destination" ]]; then
        echo "$destination is already present."
        return 0
    else
        mkdir -p "$(dirname "$destination")" 
        ln -s "$source" "$destination"
        echo "---"
        echo "Created $destination."
        echo "---"
    fi
}

## Usage: copy_file SRC DST
copy_file () {
    local source=$1
    local destination=$2

    if [[ ! -e "$source" ]]; then
        echo "$source is not present!"
        return 1
    fi

    if [[ -e "$destination" ]]; then
        echo "$destination is already present."
        return 0
    else
        mkdir -p "$(dirname "$destination")" 
        cp -i "$source" "$destination"
        echo "---"
        echo "Created $destination."
        echo "---"
    fi
}

## Usage: clone_repo SRC DST
clone_repo () {
    local source=$1
    local destination=$2

    if [[ -e "$destination" ]]; then
        echo "$destination is already present."
        return 0
    else
        git clone "$source" "$destination"
        echo "---"
        echo "Created $destination."
        echo "---"
    fi
}

link_file "$SCRIPT_DIR"/.bashrc                    ~/.bashrc
link_file "$SCRIPT_DIR"/.emacs.d                   ~/.emacs.d
link_file "$SCRIPT_DIR"/.inputrc                   ~/.inputrc
link_file "$SCRIPT_DIR"/.tmux.conf                 ~/.tmux.conf 
link_file "$SCRIPT_DIR"/.vimrc                     ~/.vimrc
link_file "$SCRIPT_DIR"/Bash/hn-convert-epoch.sh   ~/bin/hn-convert-epoch.sh
link_file "$SCRIPT_DIR"/Bash/status-bar.sh         ~/bin/status-bar.sh
link_file "$SCRIPT_DIR"/Bash/hn-compress-images.sh ~/bin/hn-compress-images.sh
link_file "$SCRIPT_DIR"/Bash/hn-resize-images.sh   ~/bin/hn-resize-images.sh
link_file "$SCRIPT_DIR"/Bash/hn-timestamp.sh       ~/bin/hn-timestamp.sh
link_file "$SCRIPT_DIR"/Bash/hn-firefox.sh         ~/bin/hn-firefox.sh
link_file "$SCRIPT_DIR"/Bash/hn-chromium.sh        ~/bin/hn-chromium.sh
link_file "$SCRIPT_DIR"/.config/nvim/init.vim      $XDG_CONFIG_HOME/nvim/init.vim
link_file "$SCRIPT_DIR"/.config/sway/config        $XDG_CONFIG_HOME/sway/config
link_file "$SCRIPT_DIR"/.config/alacritty/alacritty.yml \
          $XDG_CONFIG_HOME/alacritty/alacritty.yml
copy_file "$SCRIPT_DIR"/.gitconfig.global          ~/.gitconfig

# .emacs.d/
clone_repo "https://github.com/HarishNagisetty/verilog3-mode.git" \
    ".emacs.d/elisp/verilog3-mode/"
clone_repo "https://github.com/HarishNagisetty/origami.el.git" \
    ".emacs.d/elisp/origami.el/"
# .vim/
clone_repo "https://github.com/HarishNagisetty/vim-colors-solarized.git" \
    ~/.vim/pack/vim-colors-solarized/start/vim-colors-solarized/
clone_repo "https://github.com/HarishNagisetty/vim-rsi.git" \
    ~/.vim/pack/vim-rsi/start/vim-rsi/
clone_repo "https://github.com/HarishNagisetty/vim-gnupg.git" \
    ~/.vim/pack/vim-gnupg/start/vim-gnupg/
clone_repo "https://github.com/HarishNagisetty/verilog_systemverilog.vim.git" \
    ~/.vim/pack/verilog_systemverilog.vim/start/verilog_systemverilog.vim/

