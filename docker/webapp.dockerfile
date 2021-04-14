FROM node:12.18.3-stretch AS static_builder

RUN mkdir /server

WORKDIR /build

COPY webapp/frontend .

RUN npm install && npm run build

################################################

FROM ubuntu:18.04 AS bin_builder

RUN apt update && apt install -y \
    wget \
    sbcl \
    libmysqlclient-dev && \
    ##
    mkdir \
    /app \
    /build && \
    ##
    cd /build && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load /build/quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)"

COPY ./docker/requirements.lisp /build
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/build/requirements.lisp"


COPY --from=static_builder /server/static /app/static

COPY . /root/quicklisp/local-projects/teno
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/root/quicklisp/local-projects/teno/docker/main.lisp" \
         --eval "(sb-ext:save-lisp-and-die \
                  \"/app/webapp\" \
                  :executable t \
                  :toplevel #'teno.docker:main)"

ENTRYPOINT ["/app/webapp", "clack"]
