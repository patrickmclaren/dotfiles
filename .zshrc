# Path to your oh-my-zsh installation.
export ZSH=/home/patrick/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
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
plugins=(git fasd bundler rails last-working-dir archlinux common-aliases systemd heroku)

# User configuration

export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
export PATH="/opt/jdk/bin:$PATH"
export PATH="/opt/synergy/bin:$PATH"
export PATH="/opt/soundcloud2000/bin:$PATH"
export PATH="/home/patrick/.rbenv/shims:/home/patrick/.rbenv/bin:$PATH"
export PATH="/home/patrick/bin:$PATH"
# export MANPATH="/usr/local/man:$MANPATH"

export PATH="/opt/scala/bin:$PATH"
export SCALA_HOME="/opt/scala"

export PATH="/opt/activator/bin:$PATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

unalias run-help
autoload run-help
autoload run-help-git

alias run-help="nocorrect run-help"

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
alias alias-grep="alias G"

# Reload config
alias reload=". ~/.zshrc"

# View syslogs
alias jnl="sudo journalctl"

# Show latest screenshot
alias fehsc="feh $(ls -t1 ~ | grep scrot | head -n 1)"

# Rails server explictly bound to localhost
alias rsl="rs -b 127.0.0.1"

# TODO: What does this do?
alias rz="cd `fasd -td -2`"

alias cgrep="grep --color=always"
alias regrep="grep -rE"

alias zc="zeus start"
alias zcs="zeus server"
alias zcr="zeus rake"
alias zcc="zeus console"
alias zcsl="zcs -b 127.0.0.1"
alias zct="zeus test"
alias zctt="zeus rake teaspoon"

alias krb="pkill ruby -9"

alias hilite="highlight -O ansi"

# Syntax highlight a file, paged to `less`.
function lilite () {
  hilite "$@" | less -R
}

# Change working directory to the current ruby-gems/gems folder.
function rbg () {
  RB_VER=$(rbenv version | cut -d ' ' -f 1)
  cd "$HOME/.rbenv/versions/$RB_VER/lib/ruby/gems/$RB_VER/gems"
}

# ----------------------------------------------------------------------
# gpg-agent with ssh-agent support
# ----------------------------------------------------------------------

export GPG_TTY=$(tty)

# Check if gpg-agent is already running
pgrep -x -u "${USER}" gpg-agent >/dev/null 2>&1;
if [[ $? != 0 ]]; then
  # Agent isn't running, start new agent
  gpg-connect-agent /bye >/dev/null 2>&1
else
  # Existing agent found, update TTY
  gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1
fi

# Set envvar for gpg-agent to function as ssh-agent
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
fi
