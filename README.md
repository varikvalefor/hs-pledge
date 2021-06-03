Haskell library for OpenBSD's pledge(2)
=======================================

An example implementation is as follows:

    import System.Pledge;
    import System.IO;

    main :: IO ();
    main = pledge [Stdio] [] >>
      print "Pledge works." >>
      readFile "/dev/urandom" >>= putStr;

pledge(2) kills above example program when withFile tries to read from /dev/urandom.
