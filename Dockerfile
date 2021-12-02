# Build the jinko interpreter
# ---------------------------

FROM rust:slim-bullseye as build

COPY . /jinko
WORKDIR /jinko
RUN rustup target add x86_64-unknown-linux-musl
RUN cargo build --target=x86_64-unknown-linux-musl --release

# Run the jinko interpreter in a fresh container
# ----------------------------------------------

FROM alpine:3.15.0

COPY --from=build /jinko/target/x86_64-unknown-linux-musl/release/jinko /jinko

RUN apk add ncurses

ENTRYPOINT ["/jinko"]

LABEL maintainer="Tanguy Segarra <tanguy.segarra@epita.fr>"
