# Move to home dir
cd ~/

# Link dotfiles to the repo files
ln -sf ~/.dotfiles/dotfiles/bashrc ~/.bashrc
ln -sf ~/.dotfiles/dotfiles/tmux.conf ~/.tmux.conf
ln -sf ~/.dotfiles/dotfiles/vimrc ~/.vimrc
ln -sf ~/.dotfiles/dotfiles/Xresources ~/.Xresources
ln -sf ~/.dotfiles/dotfiles/zshrc ~/.zshrc

# Link to the tmuxinator dir
rm -rf ~/.tmuxinator
ln -sf ~/.dotfiles/dotfiles/tmuxinator ~/.tmuxinator

# Link bin files
sudo ln -sf ~/.dotfiles/dotfiles/bin/* /usr/local/bin

# Link to vim plugin autoloader
mkdir -p ~/.vim/autoload
ln -sf ~/.dotfiles/dotfiles/vim/autoload/pathogen.vim ~/.vim/autoload/pathogen.vim

# selenium server
rm -rf ~/.selenium-server
ln -sf ~/.dotfiles/dotfiles/selenium-server ~/.selenium-server

# Install vim plugins
rm -rf ~/.vim/bundle
mkdir -p ~/.vim/bundle
cd ~/.dotfiles
git submodule init
git submodule update
ln -sf ~/.dotfiles/dotfiles/vim/bundle/* ~/.vim/bundle
cd ~/
echo "All dotfiles have been installed"
