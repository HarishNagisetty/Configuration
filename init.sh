#! /usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}

## Usage: link_file SRC DST
link_file () {
    local source=$1
    local destination=$2

    if [[ ! -e "$source" ]]; then
        echo "$source is not a file!"
        return 1
    fi

    if [[ -e "$destination" ]]; then
        echo "$destination is already present."
        return 0
    else
        mkdir -p "$(dirname "$destination")" 
        ln -s "$source" "$destination"
    fi
}

link_file "$SCRIPT_DIR"/.bashrc                  ~/.bashrc
touch                                            ~/.bashrc.private
link_file "$SCRIPT_DIR"/.emacs.d                 ~/.emacs.d
link_file "$SCRIPT_DIR"/.inputrc                 ~/.inputrc
link_file "$SCRIPT_DIR"/.tmux.conf               ~/.tmux.conf 
link_file "$SCRIPT_DIR"/.vimrc                   ~/.vimrc

link_file "$SCRIPT_DIR"/Bash/hn-convert-epoch.sh ~/bin/hn-convert-epoch.sh
link_file "$SCRIPT_DIR"/Bash/status-bar.sh       ~/bin/status-bar.sh

link_file "$SCRIPT_DIR"/.config/nvim/init.vim    $XDG_CONFIG_HOME/nvim/init.vim
link_file "$SCRIPT_DIR"/.config/sway/config      $XDG_CONFIG_HOME/sway/config

printf "#! /usr/bin/env bash\nfirefox -private-window\n" > ~/bin/firefly
chmod +x ~/bin/firefly

printf '#! /usr/bin/env bash\nchromium-browser --incognito --user-data-dir=/tmp/$(date +%s)\n' > ~/bin/chromium-private
chmod +x ~/bin/chromium-private

