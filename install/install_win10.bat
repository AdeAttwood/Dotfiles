git clone https://github.com/AdeAttwood/Dotfiles.git %USERPROFILE%\_dotfiles

:: vim

chdir %homepath%\_dotfiles
git submodule init 
git submodule update
chdir %homepath%

mkdir %homepath%\vimfiles
mklink /D %homepath%\vimfiles\bundle %homepath%\_dotfiles\dotfiles\vim\bundle
mklink /D %homepath%\vimfiles\autoload %homepath%\_dotfiles\dotfiles\vim\autoload
mklink %homepath%\_vimrc %homepath%\_dotfiles\dotfiles\vimrc

