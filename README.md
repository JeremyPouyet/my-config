# mybashrc
My .bashrc and files that come with it

After losing all these precious files that makes you use bash better several times, I decided to open this safety repository. Maybe some of you will find useful things in it.

.mybashrc -> my conf, so the default conf is intact.  
.bash_functions -> some useful functions to save some times.  
.bash_alias -> my aliases, the vast majority are used to save times and have a clean terminal.  

To have a good use of my conf, I usually add this code to the default .bashrc:

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
