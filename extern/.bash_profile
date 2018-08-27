### On windows, if you are using git bash with M-x shell,
### copy this into .bash_profile in your git bash $HOME.
###
### Source: https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
if [ -n "$INSIDE_EMACS" ]; then
    export PS1='\[\033[32m\]\u@\h \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ '
fi
