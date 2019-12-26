ARG VERSION=19.04
FROM ubuntu:$VERSION

MAINTAINER JAremko <w3techplaygound@gmail.com>

# Fix "Couldn't register with accessibility bus" error message
ENV NO_AT_BRIDGE=1

ENV DEBIAN_FRONTEND noninteractive

# basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
    && apt-get update && apt-get install \
    bash \
    build-essential \
    dbus-x11 \
    fontconfig \
    git \
    gzip \
    language-pack-en-base \
    libgl1-mesa-glx \
    make \
    sudo \
    tar \
    unzip \
    python3 \
    python3-pip \
# data utils
    && pip3 install nltk catboost lightgbm xgboost imblearn graphviz \
    && python3 -m nltk.downloader all \
# su-exec
    && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
    && cd /tmp/su-exec \
    && make \
    && chmod 770 su-exec \
    && mv ./su-exec /usr/local/sbin/ \
# Cleanup
    && apt-get purge build-essential \
    && apt-get autoremove \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

# Emacs
RUN apt-get update && apt-get install software-properties-common \
    && apt-add-repository ppa:kelleyk/emacs \
    && apt-get update && apt-get install emacs26 \
# Cleanup
    && apt-get purge software-properties-common \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

COPY asEnvUser /usr/local/sbin/asEnvUser
COPY emacs /opt/emacs
COPY emacs-instalation /opt/emacs-instalation
COPY lain /opt/lain

# Only for sudoers
RUN chown root /usr/local/sbin/asEnvUser \
    && chmod 700  /usr/local/sbin/asEnvUser

# ^^^^^^^ Those layers are shared ^^^^^^^

ENV UNAME="emacser" \
    GNAME="emacs" \
    UHOME="/home/emacs" \
    UID="1000" \
    GID="1000" \
    WORKSPACE="/mnt/workspace" \
    SHELL="/bin/bash"

WORKDIR "${WORKSPACE}"

ENTRYPOINT ["asEnvUser"]
CMD ["emacsclient -c"]
