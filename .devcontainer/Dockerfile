FROM node:20-trixie

ARG TZ="Europe/London"
ENV TZ="$TZ"

ARG CLAUDE_CODE_VERSION=latest

# Install basic development tools and iptables/ipset
RUN apt-get update && apt-get install -y --no-install-recommends \
  less \
  git \
  procps \
  sudo \
  fzf \
  zsh \
  man-db \
  unzip \
  gnupg2 \
  gh \
  iptables \
  ipset \
  iproute2 \
  dnsutils \
  aggregate \
  jq \
  nano \
  vim \
  locales \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# Configure locales
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Ensure default node user has access to /usr/local/share
RUN mkdir -p /usr/local/share/npm-global && \
  chown -R node:node /usr/local/share

ARG USERNAME=node

# Persist bash history.
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
  && mkdir /commandhistory \
  && touch /commandhistory/.bash_history \
  && chown -R $USERNAME /commandhistory

# Set `DEVCONTAINER` environment variable to help with orientation
ENV DEVCONTAINER=true

# Create workspace and config directories and set permissions
RUN mkdir -p /workspace /home/node/.claude && \
  chown -R node:node /workspace /home/node/.claude

WORKDIR /workspace

ARG GIT_DELTA_VERSION=0.18.2
RUN ARCH=$(dpkg --print-architecture) && \
  wget "https://github.com/dandavison/delta/releases/download/${GIT_DELTA_VERSION}/git-delta_${GIT_DELTA_VERSION}_${ARCH}.deb" && \
  sudo dpkg -i "git-delta_${GIT_DELTA_VERSION}_${ARCH}.deb" && \
  rm "git-delta_${GIT_DELTA_VERSION}_${ARCH}.deb"

ADD https://raw.githubusercontent.com/newren/git-filter-repo/refs/heads/main/git-filter-repo /usr/bin/git-filter-repo
RUN chmod a+x /usr/bin/git-filter-repo

ARG YAZI_VERSION=0.4.2
RUN ARCH=$(dpkg --print-architecture | sed 's/amd64/x86_64/;s/arm64/aarch64/') && \
  wget -q "https://github.com/sxyazi/yazi/releases/download/v${YAZI_VERSION}/yazi-${ARCH}-unknown-linux-musl.zip" && \
  unzip -q "yazi-${ARCH}-unknown-linux-musl.zip" && \
  mv "yazi-${ARCH}-unknown-linux-musl/yazi" /usr/local/bin/ && \
  mv "yazi-${ARCH}-unknown-linux-musl/ya" /usr/local/bin/ && \
  rm -rf "yazi-${ARCH}-unknown-linux-musl"*

# Set up non-root user
USER node

# Install global packages
ENV NPM_CONFIG_PREFIX=/usr/local/share/npm-global
ENV PATH=$PATH:/usr/local/share/npm-global/bin

# Set the default shell to zsh rather than sh
ENV SHELL=/bin/zsh

# Set the default editor and visual
ENV EDITOR=vim
ENV VISUAL=vim

# Default powerline10k theme
ARG ZSH_IN_DOCKER_VERSION=1.2.0
RUN sh -c "$(wget -O- https://github.com/deluan/zsh-in-docker/releases/download/v${ZSH_IN_DOCKER_VERSION}/zsh-in-docker.sh)" -- \
  -p git \
  -p fzf \
  -a "source /usr/share/doc/fzf/examples/key-bindings.zsh" \
  -a "source /usr/share/doc/fzf/examples/completion.zsh" \
  -a "export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
  -x

# Install Claude
# RUN npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}
RUN curl -fsSL https://claude.ai/install.sh | bash

COPY .devcontainer/setup-ocaml.sh /usr/local/bin

# Copy and set up firewall script and let node sudo to all (for opam depexts)
COPY .devcontainer/init-firewall.sh /usr/local/bin/
USER root
RUN chmod +x /usr/local/bin/init-firewall.sh && \
  echo "node ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/node-firewall && \
  chmod 0440 /etc/sudoers.d/node-firewall
USER node
RUN /usr/local/bin/setup-ocaml.sh

# Pin packages that need dev versions from git
RUN eval $(opam env) && \
  opam pin add -ny opentelemetry git+https://github.com/ocaml-tracing/ocaml-opentelemetry.git && \
  opam pin add -ny opentelemetry-client git+https://github.com/ocaml-tracing/ocaml-opentelemetry.git && \
  opam pin add -ny opentelemetry-client-cohttp-eio git+https://github.com/ocaml-tracing/ocaml-opentelemetry.git

# Pre-install project dependencies (cached unless opam files change)
COPY --chown=node:node ./oystermark.opam ./trace-collect.opam ./dune-project /tmp/opam-deps/
RUN cd /tmp/opam-deps && eval $(opam env) && \
  opam install . --deps-only --with-test --with-doc -y && \
  opam clean -a && \
  rm -rf /tmp/opam-deps
