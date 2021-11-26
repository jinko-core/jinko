# Build the jinko interpreter
# ---------------------------

FROM rust:slim-bullseye as build

COPY . /jinko
WORKDIR /jinko
RUN ./install.sh

# ENTRYPOINT ["/root/.jinko/bin/jinko"]

# Run the jinko interpreter in a fresh container
# ----------------------------------------------

FROM archlinux:latest

COPY --from=build /root/.jinko/bin/jinko /jinko

ENTRYPOINT ["/jinko"]

LABEL maintainer="Tanguy Segarra <tanguy.segarra@epita.fr>"
