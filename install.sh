#! /bin/bash

ubuntu_setup() {
    sudo apt update
    sudo apt -y upgrade
    sudo apt install -y git opam
    sudo snap install emacs --classic
    sudo snap install slack
    sudo apt-get install texlive-full
    sudo apt install fish
}

mac_setup() {
    xcode-select --install > /dev/null
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" > /dev/null
    brew install git
    brew install opam
    brew install --cask emacs
    brew install --cask mactex
    sudo tlmgr update --self --all
    sudo tlmgr paper a4
    brew install fish
}

common_setup() {
    # git
    git config --global user.name "Yuta Natsui"
    git config --global user.email "xiajingyoutai@gmail.com"

    # opam
    opam init -a
    eval $(opam env --switch=default)
    opam install -y ocaml-lsp-server ocamlformat utop dune menhir
}

if [ "$(uname)" == 'Darwin' ]; then
    mac_setup
    common_setup
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    ubuntu_setup
    common_setup
else
    echo "Your platform is not supported."
    exit 1
fi
