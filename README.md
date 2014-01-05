BridgeBuddyServer
=================

A JSON RPC Server for Bridge Buddy

BridgeBuddyServer is the back-end for BridgeBuddy, an Android and iOS app for helping those
learning the card game Bridge, especially in ACOL bidding and leads.

Spec
----

The ACOL bidding system used is described in the [No Fear Bridge Crib Sheet](http://www.nofearbridge.co.uk/crib_sheet_new.pdf)

Setting Up
----------

```
cabal install
```

Running
-------

To test a single hand, 

```
cabal run BridgeBuddy N
```

where N = number of hands you want to display.

For the JSON server, the bridge hands, bids and resons for the bid are stored 
in a Beanstalk queue in JSON format. You need a running instance of Beanstalk for this.

```
cabal run populate_weighted_queue 30000
```

To run the server:


```
cabal run BridgeBuddyServer
```

Open a browser and go to: http://localhost:3000/testjsonqueue


