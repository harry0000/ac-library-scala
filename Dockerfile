FROM eclipse-temurin:21-jdk

ARG SBT_VERSION=1.11.7
ARG SCALA_VERSION=3.7.2

ENV SBT_VERSION=${SBT_VERSION} \
    SCALA_VERSION=${SCALA_VERSION} \
    SBT_HOME=/usr/share/sbt \
    PATH=/usr/share/sbt/bin:${PATH}

# Install required tools for Scala Native, sbt, and general build utilities
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        curl \
        gnupg \
        ca-certificates \
        bash \
        build-essential \
        clang \
        lld \
        libclang-dev \
        libunwind-dev \
        libgc-dev \
        zlib1g-dev \
        pkg-config \
        libssl-dev \
        git \
    && rm -rf /var/lib/apt/lists/*

# Add sbt APT repo (with signed-by)
RUN curl -fsSL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" \
        | gpg --dearmor -o /usr/share/keyrings/sbt.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" \
        > /etc/apt/sources.list.d/sbt.list

# Install sbt
RUN apt-get update \
    && apt-get install -y --no-install-recommends "sbt=${SBT_VERSION}" \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

CMD ["sbt"]
