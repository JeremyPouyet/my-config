# my-config
My emacs, bash and git config

After losing all these precious files that makes you use Linux better several times, I decided to open this safety repository. Maybe some of you will find useful things in it.

* git/ -> contains my git config. Nothing awsome but I like it!
* emacs/ -> my precious emacs config that I like so much!
* bash/.bash_functions -> some useful functions to save some times.
* bash/.bash_alias -> my aliases, the vast majority are used to save times and have a clean terminal.

To have a good use of my bash conf, I usually add this code to the default .bashrc:

```
#load personal conf
if [ -f ~/.mybashrc ]; then
   source ~/.mybashrc
fi

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
   . ~/.bash_aliases
fi

# functions declarations
if [ -f ~/.bash_functions ]; then
   . ~/.bash_functions
fi

```

To take advantage of my custom git diff, you'll need the npm package diff-so-fancy so:

`npm install -g diff-so-fancy`
