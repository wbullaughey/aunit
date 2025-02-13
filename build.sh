echo build.sh at `pwd`
source ~/.zshrc
#export GPR_PROJECT_PATH="ada_lib:aunit"
alr -v build -- -j10 -s -k -gnatE
