FROM images.reesd.com/reesd/stack

ENV PATH /home/gusdev/.cabal/bin:$PATH
RUN cabal update
RUN cabal fetch haxr-3000.10.3
RUN tar xf .cabal/packages/hackage.reesd.com/haxr/3000.10.3/haxr-3000.10.3.tar.gz \
  -C .cabal/packages/hackage.reesd.com/haxr/3000.10.3/
RUN sed -ie 's/network < 3/network == 2.4.*/' \
  .cabal/packages/hackage.reesd.com/haxr/3000.10.3/haxr-3000.10.3/haxr.cabal
RUN cabal install .cabal/packages/hackage.reesd.com/haxr/3000.10.3/haxr-3000.10.3/haxr.cabal
