# emacs-config
## Biagio's Emacs Configuration

[![Docker](https://img.shields.io/badge/Docker-yes-blue.svg?logo=docker)](https://hub.docker.com/r/biagiofesta/emacs-config)

---

## Linux Installation

~~~
git clone --recursive https://github.com/BiagioFesta/emacs-config.git && \
cd emacs-config && \
./install-linux.sh
~~~

## Docker

### Download Image
~~~
docker pull biagiofesta/emacs-config
~~~

### Temporary container
~~~
docker run -it --rm -e TERM=${TERM} biagiofesta/emacs-config
~~~
