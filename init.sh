#! /usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
FUNCTIONS="$SCRIPT_DIR/functions.sh"

if [ -f "$FUNCTIONS" ]; then
    source "$FUNCTIONS"
fi

link_file "$SCRIPT_DIR"/.bashrc                    "$HOME"/.bashrc
link_file "$SCRIPT_DIR"/.cshrc                     "$HOME"/.cshrc
link_file "$SCRIPT_DIR"/.emacs.d                   "$HOME"/.emacs.d
link_file "$SCRIPT_DIR"/.inputrc                   "$HOME"/.inputrc
link_file "$SCRIPT_DIR"/.tmux.conf                 "$HOME"/.tmux.conf
link_file "$SCRIPT_DIR"/.vimrc                     "$HOME"/.vimrc
link_file "$SCRIPT_DIR"/.config/nvim/init.vim      "$XDG_CONFIG_HOME"/nvim/init.vim
copy_file "$SCRIPT_DIR"/.gitconfig.global          "$HOME"/.gitconfig
link_file "$SCRIPT_DIR"/Bash/hn-convert-epoch.sh   "$HOME"/bin/hn-convert-epoch.sh
link_file "$SCRIPT_DIR"/Bash/hn-firefox.sh         "$HOME"/bin/hn-firefox.sh
link_file "$SCRIPT_DIR"/Bash/hn-chromium.sh        "$HOME"/bin/hn-chromium.sh
link_file "$SCRIPT_DIR"/Csh/hn-tmpdir.csh          "$HOME"/bin/hn-tmpdir.csh

# .emacs.d/
clone_repo "https://github.com/HarishNagisetty/verilog3-mode.git" \
    ".emacs.d/elisp/verilog3-mode/"
clone_repo "https://github.com/HarishNagisetty/origami.el.git" \
    ".emacs.d/elisp/origami.el/"
clone_repo "https://github.com/nashamri/spacemacs-theme.git" \
    ".emacs.d/elisp/spacemacs-theme/"
# .vim/
clone_repo "https://github.com/HarishNagisetty/vim-colors-solarized.git" \
    "$HOME"/.vim/pack/vim-colors-solarized/start/vim-colors-solarized/
clone_repo "https://github.com/HarishNagisetty/vim-rsi.git" \
    "$HOME"/.vim/pack/vim-rsi/start/vim-rsi/
clone_repo "https://github.com/HarishNagisetty/vim-gnupg.git" \
    "$HOME"/.vim/pack/vim-gnupg/start/vim-gnupg/
clone_repo "https://github.com/HarishNagisetty/verilog_systemverilog.vim.git" \
    "$HOME"/.vim/pack/verilog_systemverilog.vim/start/verilog_systemverilog.vim/

