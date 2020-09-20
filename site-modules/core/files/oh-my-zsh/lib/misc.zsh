#
# Overrode to he misc lib file to manipulate oh my zsh before the plugins are
# loaded. If you need to run code after the plugins you can see
# $ZSH_CUSTOM/custom.zsh
#

#
# Load in the original misc file
#
source $ZSH/lib/misc.zsh

#
# Add all of the plugins in the custom plugins dir automatically with out
# defining them in the pugins array
#
for plugin ($ZSH_CUSTOM/plugins/*); do
    plugin_name="$(basename $plugin)"
    if [[ ! -n "${plugins[(r)$plugin_name]}" ]]; then
        plugins+=($plugin_name)
    fi
done
unset plugin plugin_name
