FROM        ubuntu:noble
MAINTAINER  Mu Lei (mulei@gnu.org)
ENV         LANG C.UTF-8
RUN     apt update -y \
        && apt install --no-install-recommends -y gettext texinfo guile-3.0 guile-3.0-dev make automake git autoconf libtool figlet boxes lolcat \
        && rm -rf /var/lib/apt/lists/* \
        && git config --global http.sslverify false

ARG CACHE_LACO=1
RUN     git clone https://github.com/hardenedlinux/laco.git \
        && cd laco \
        && autoreconf -i -f \
        && aclocal \
        && autoconf \
        && automake --add-missing \
	&& ./configure --prefix=/usr \
	&& make \
        && make install \
        && cd .. \
        && rm -fr laco

ARG CACHE_MISC=1
RUN     useradd -s /bin/bash -m animula \
        && echo "figlet -f mini Animula | boxes -d parchment | /usr/games/lolcat -a -s 200" >> /home/animula/.bashrc \
        && echo "echo 'Laco compiler workspace' | /usr/games/lolcat -a -s 200" >> /home/animula/.bashrc
