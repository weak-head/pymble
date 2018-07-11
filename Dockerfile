FROM haskell:8

EXPOSE 23
WORKDIR /opt/server

RUN stack update

# Copy package.yaml and stack.yaml to capture dependencies
COPY ./package.yaml /opt/server/package.yaml
COPY ./stack.yaml /opt/server/stack.yaml

# Docker will cache this layer, so we can modify
# source code without re-installing dependencies
# (unless the stack.yaml or package.yaml changes)
RUN stack -j2 setup --no-terminal \
    && stack -j2 build --only-snapshot --no-terminal

# Copy, build and install pymble
COPY . /opt/server
RUN stack -j2 build --no-terminal \
    && stack install

CMD [ "pymble" ]