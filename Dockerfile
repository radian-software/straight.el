ARG BASE=silex/emacs
ARG VERSION=30
FROM $BASE:$VERSION

COPY scripts/docker-install.bash /tmp/
RUN /tmp/docker-install.bash

WORKDIR /src

CMD bash
