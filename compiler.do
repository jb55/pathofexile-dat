GHC_OPTS=-Wall
echo "#!/bin/sh" > $3
echo "cabal-dev \$@" >> $3
chmod +x $3
