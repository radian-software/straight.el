ARG BASE=silex/emacs
ARG VERSION=30
FROM $BASE:$VERSION

ARG UID

COPY scripts/docker-install.bash /tmp/
RUN /tmp/docker-install.bash "$UID"

USER $UID
WORKDIR /home/docker/src

CMD bash
