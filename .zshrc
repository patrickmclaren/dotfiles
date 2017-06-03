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
plugins=(archlinux common-aliases docker docker-compose fasd git heroku last-working-dir systemd)

# User configuration

# Reset PATH to something sane
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin"

# Prepend some directories to PATH for snappier shell + terminal
export PATH="/home/patrick/.rbenv/shims:/home/patrick/.rbenv/bin:$PATH"
export PATH="/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:$PATH"

# Append other directories to PATH
export PATH="$PATH:/opt/jdk/bin:/opt/scala/bin:/opt/activator/bin:/opt/synergy/bin:/home/patrick/bin"

# export MANPATH="/usr/local/man:$MANPATH"

export JAVA_HOME="/opt/jdk"
export SCALA_HOME="/opt/scala"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

unalias run-help
autoload run-help
autoload run-help-git

alias run-help="nocorrect run-help"

# Preferred editor for local and remote sessions
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
alias dcr="nocorrect docker-compose run"

alias fmst="PORT=3000 foreman start"

# Learn about existing aliases
alias alias-grep="alias G"

alias agctx="ag -B 4 -A 4"

# Reload config
alias reload=". ~/.zshrc"

# Show latest screenshot
alias fehsc="feh $(ls -t1 ~ | grep scrot | head -n 1)"

# TODO: What does this do?
alias rz="cd `fasd -td -2`"

alias cgrep="grep --color=always"
alias regrep="grep -rE"

alias krb="pkill ruby -9"

alias hilite="highlight -O ansi"

alias tcpu="top -o '+%CPU' -c"

# Syntax highlight a file, paged to `less`.
function lilite () {
  hilite "$@" | less -R
}

# Change working directory to the current ruby-gems/gems folder.
function rbg () {
  local rb_ver=$(rbenv version | cut -d ' ' -f 1)
  cd "$HOME/.rbenv/versions/$rb_ver/lib/ruby/gems/$rb_ver/gems"
}

# Copies the hash of the last commit for a given file into the X11 clipboard.
function gcph () {
  local gcph_args="$@"
  local git_log_entry=$(git --no-pager log -n 1 --oneline "$gcph_args")

  if [[ $! == 0 ]]; then
    local git_commit_hash=$(echo "$git_log_entry" | cut -d ' ' -f 1)

    if [[ $! == 0 ]]; then
      echo "$git_commit_hash" | xclip
      echo "Copied $git_commit_hash to the X11 Clipboard"
    else
      echo "Couldn't locate a commit hash in:"
      echo "$git_log_entry"
    fi
  else
    echo "No log entries found for $gcph_args"
  fi
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
