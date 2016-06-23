FROM haskell:7.10.3

MAINTAINER Frédéric Menou <frederic.menou@gmail.com>

# App user
RUN useradd -m app
RUN  chown -R app:app /home/app/

# Application working directory
USER app
WORKDIR /home/app

# Dependencies
COPY stack.yaml /home/app/stack.yaml
COPY yummy.cabal /home/app/yummy.cabal
USER root
RUN  chown -R app:app /home/app/
USER app
RUN stack build --only-snapshot

ENV PATH /home/app/.local/bin:$PATH

# Copy code
COPY ./ /home/app/
USER root
RUN  chown -R app:app /home/app/
USER app

# Build
RUN stack install

EXPOSE 8080
ENTRYPOINT yummy-exe
