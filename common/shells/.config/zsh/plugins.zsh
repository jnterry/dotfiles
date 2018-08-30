pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

source ${SCRIPTDIR}/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source ${SCRIPTDIR}/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
