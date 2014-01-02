all: 
	ghc -no-user-package-db -package-db .cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d populate_weighted_queue.hs
	ghc -no-user-package-db -package-db .cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d Cards.hs BridgeBuddy.hs 
	ghc -no-user-package-db -package-db .cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d Cards.hs ScottyWebServer.hs

clean: 
	rm *.o *.hi
	rm bridgebuddy populate_weighted_queue ScottyWebServer
