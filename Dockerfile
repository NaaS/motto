FROM ocaml/opam:alpine
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n motto /home/opam/src
RUN opam depext -uy --noninteractive motto
RUN opam install -j 2 -v -y motto
VOLUME /src
WORKDIR /src
ENTRYPOINT ["opam","config","exec","--","motto"]
