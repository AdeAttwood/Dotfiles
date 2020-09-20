#
# Set up
#
SCRIPT_PATH="${0:A:h}"

#
# Source all the custom lib files
#
for config_file ($ZSH_CUSTOM/custom/*.zsh); do
  source $config_file
done
unset config_file
