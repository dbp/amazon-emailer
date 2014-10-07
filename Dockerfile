FROM dbp1/ghc-7.8.3-migrate-rivet
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install zlib1g-dev libssl-dev -y
RUN apt-get install git -y
RUN apt-get install libpq-dev postgresql-client -y
ENV LANG en_US.utf8
ADD ./amazon-emailer.cabal /srv/amazon-emailer.cabal
RUN cabal update
RUN cabal install angel
ADD ./LICENSE /srv/LICENSE
RUN cd /srv && cabal install --force-reinstalls --only-dependencies
ADD ./Mailer.hs /srv/Mailer.hs
ADD ./Cleaner.hs /srv/Cleaner.hs
ADD ./Setup.hs /srv/Setup.hs
RUN cd /srv && cabal install --force-reinstalls
ADD ./angel.conf /srv/angel.conf
CMD /root/.cabal/bin/angel /srv/angel.conf
# NOTE(dbp 2014-10-07): Run like:
# docker run -d -w /srv  -v /srv/emailer.cfg:/srv/prod/emailer.cfg -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432  -t -i dbp1/amazon_emailer