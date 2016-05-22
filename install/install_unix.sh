# Move to home dir
cd ~/

# Link dotfiles to the repo files
ln -sf ~/.dotfiles/dotfiles/bashrc ~/.bashrc
ln -sf ~/.dotfiles/dotfiles/tmux.conf ~/.tmux.conf
ln -sf ~/.dotfiles/dotfiles/vimrc ~/.vimrc
ln -sf ~/.dotfiles/dotfiles/Xresources ~/.Xresources

# Link to the tmuxinator dir
ln -sf ~/.dotfiles/dotfiles/tmuxinator ~/.tmuxinator

# Make vim dir tree
mkdir ~/.vim/autoload ~/.vim/bundle/
# Link to vim autoload plugin
ln -sf ~/.dotfiles/dotfiles/vim/autoload/pathogen.vim ~/.vim/autoload/pathogen.vim

