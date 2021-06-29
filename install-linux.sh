#!/usr/bin/env bash
EMACS_HOME=$(realpath "${HOME}/.emacs.d")
EMACS_BIN=$(which emacs)

if [ -z ${EMACS_BIN} ]; then
    echo "Error: Emacs binary not found."
    exit -1
fi

echo "Emacs installation dir: ${EMACS_HOME}"

if [ -d ${EMACS_HOME} ]; then
    read -r -p "'${EMACS_HOME}' already exists. Do you want to override it? [y/N]: " ans
    case "$ans" in
        [yY])
        ;;
        *)
            echo "Installation aborted"
            exit -1
    esac
fi

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

rm -rf ${EMACS_HOME}
mkdir -p ${EMACS_HOME}
ln -s ${DIR}/init.el ${EMACS_HOME}/
ln -s ${DIR}/early-init.el ${EMACS_HOME}/
ln -s ${DIR}/core/ ${EMACS_HOME}/
ln -s ${DIR}/etc/ ${EMACS_HOME}/
echo "Configuration files installed."

read -r -p "Launching Emacs script and complete the installation? [Y/n]: " ans
case "$ans" in
    [nN])
        echo "Done!"
        exit 0;;
    *)
esac

echo "Configuring Emacs..."
${EMACS_BIN} --script ${EMACS_HOME}/init.el
echo "Done!"
