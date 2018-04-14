{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import "base" Data.Functor ( void )

import "network-transport-tcp" Network.Transport.TCP (createTransport, defaultTCPParameters)
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Node


main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" ("localhost",) defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    void $ runProcess node $ do
        -- get our own process id
        self <- getSelfPid
        liftIO $ putStrLn $ "pid: " ++ show self
        send self "hello"
        hello <- expect @String
        liftIO $ putStrLn hello

