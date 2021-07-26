FROM archlinux:latest
LABEL maintainer="Biagio Festa"

ENV EMACS_CONFIG_REPO https://github.com/BiagioFesta/emacs-config.git

RUN pacman -Syyu --noconfirm
RUN pacman -S --noconfirm git emacs which xsel aspell aspell-en aspell-it
RUN pacman -S --noconfirm gcc gcc-libs clang llvm llvm-libs cmake make ninja ccls
RUN pacman -S --noconfirm python python-pip pyenv
RUN pip install --upgrade jedi epc pipenv virtualenv flake8 pylint

RUN cd /opt && \
  git clone --recursive ${EMACS_CONFIG_REPO} && \
  cd emacs-config && \
  echo "Y" | ./install-linux.sh

ENTRYPOINT ["emacs"]
