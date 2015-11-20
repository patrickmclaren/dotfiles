# Path to your oh-my-zsh installation.
export ZSH=/home/patrick/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="wedisagree"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git fasd bundler rails last-working-dir colored-man archlinux common-aliases systemd)

# User configuration

export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
export PATH="/opt/jdk/bin:$PATH"
export PATH="/opt/scala/bin:$PATH"
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/home/patrick/.rbenv/shims:/home/patrick/.rbenv/bin:$PATH"
# export MANPATH="/usr/local/man:$MANPATH"

export SCALA_HOME="/opt/scala"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacsclient -t'
else
  export EDITOR='emacsclient -c'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias fmst="PORT=3000 foreman start"

# Learn about existing aliases
alias aG="alias G"

# Reload config
alias reload=". ~/.zshrc"

# Play Activator
alias activator="/opt/activator/activator"

# View syslogs
alias jnl="sudo journalctl"

# PHP code analysis
alias codegraph="/opt/share/pfff/codegraph -lang php -class_analysis -derived_data -build ."
alias scheck="/opt/share/pfff/scheck"
alias composer="php ~/composer.phar"

# OCaml Package Manager configuration
. /home/patrick/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Show latest screenshot
alias fehsc="feh $(ls -t1 ~ | grep scrot | head -n 1)"

# Rails server explictly bound to localhost
alias rsl="rs -b 127.0.0.1"

# Grep w/ color
alias cgrep="grep --color=always"

# Syntax highlight a file in less
function pless () {
  pygmentize ${@:2} "$1" | less -R
}

eval `keychain --noask --quiet --eval`
