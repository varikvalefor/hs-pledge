Haskell library for OpenBSD's pledge(2)
=======================================

An example implementation is as follows:

    import System.Pledge
    import System.IO

    main :: IO ()
    main = do
      pledge [Stdio] []
      print "Pledge works."
      withFile "/dev/urandom" ReadMode $ \f ->
        hGetContents f >>= putStr

pledge(2) kills the program when withFile tries to read from /dev/urandom.
