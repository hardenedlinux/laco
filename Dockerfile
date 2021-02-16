FROM        debian:sid-slim
MAINTAINER  Mu Lei
ENV         LANG C.UTF-8
RUN     apt-get update \
        && apt-get install -y texinfo guile-3.0 guile-3.0-dev build-essential automake git autoconf libtool \
        && rm -rf /var/lib/apt/lists/*

ARG CACHE_LACO=1
RUN     git clone --depth 1 --single-branch --branch master https://gitlab.com/lambdachip/laco.git \
        && cd laco \
	&& ./configure --prefix=/usr \
	&& make \
        && make install \
        && cp -f scripts/laco /bin/ \
        && cd .. \
        && rm -fr laco

