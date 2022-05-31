{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Socket.Extended
    ( module Exports
    , openSocket
    , closeSocket
    , getAddrFamily
    , getAddrPort
    , readListenString
    ) where

import qualified Network.Socket as Exports

import Network.Socket
    ( AddrInfo(AddrInfo)
    , Family(AF_UNIX, AF_INET, AF_INET6)
    , SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix)
    , Socket
    , SocketType(Stream)
    , bind
    , close
    , defaultProtocol
    , getAddrInfo
    , getSocketName
    , listen
    , maxListenQueue
    , socket
    )
import Network.URI
import Network.Wai.Handler.Warp (Port)
import System.Posix.Files.Extended (removeIfExists)

openSocket :: SockAddr -> IO Socket
openSocket sockAddr = do
    let family = getAddrFamily sockAddr
    case sockAddr of
        SockAddrUnix sockFile -> removeIfExists sockFile
        _ -> return ()
    socket family Stream defaultProtocol

closeSocket :: Socket -> IO ()
closeSocket sock = do
  name <- getSocketName sock
  close sock
  case name of
    SockAddrUnix sockFile -> removeIfExists sockFile
    _ -> return ()

getAddrFamily :: SockAddr -> Family
getAddrFamily = \case
    SockAddrInet _ _ -> AF_INET
    SockAddrInet6 _ _ _ _ -> AF_INET6
    SockAddrUnix _ -> AF_UNIX
    sockAddr -> error ("getAddrFamily: don't know family of " <> show sockAddr)

getAddrPort :: SockAddr -> Port
getAddrPort = \case
    SockAddrInet portNumber _ -> fromIntegral portNumber
    SockAddrInet6 portNumber _ _ _ -> fromIntegral portNumber
    _ -> 0

readListenString :: String -> IO (Either String SockAddr)
readListenString cListen =
    case parseURI cListen of
      Just URI{..} ->
        case uriScheme of
          "inet:" ->
            case uriAuthority of
              Just URIAuth{..} -> do
                let
                    hostName =
                      if uriRegName == "" then
                        Nothing
                      else
                        Just uriRegName
                    serviceName =
                      if uriPort == "" then
                        Nothing
                      else
                        Just (tail uriPort)
                AddrInfo{..}:_ <-
                    getAddrInfo Nothing hostName serviceName
                return (Right addrAddress)
              Nothing ->
                return (Left "could not parse inet listen string")
          "unix:" ->
            return (Right (SockAddrUnix uriPath))
          invalid ->
            return (Left ("unsupported listen scheme: " <> invalid))
      Nothing ->
        return (Left "could not parse listen string")

