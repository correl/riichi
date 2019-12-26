FROM erlang:19.3 as builder
MAINTAINER Correl Roush <correl@gmail.com>

WORKDIR /home/build
RUN apt-get update \
    && apt-get -y install curl gnupg \
    && curl -sL https://deb.nodesource.com/setup_11.x  | bash - \
    && apt-get -y install nodejs
COPY . /home/build
RUN rebar3 as prod release

FROM erlang:19.3-slim
WORKDIR /home/app
ENV RELX_REPLACE_OS_VARS true
COPY --from=builder /home/build/_build/prod/rel/riichi /home/app
EXPOSE 8080 8080
ENTRYPOINT ["/home/app/bin/riichi"]
