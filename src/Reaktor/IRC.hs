{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.IRC
    ( Command(..)
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Read as T
import GHC.Generics (Generic)
import Prelude.Extended

data Command =
    UnknownCommand Text | UnknownReply Int
    | ADMIN
    | AUTHENTICATE
    | AWAY
    | CAP
    | CONNECT
    | DIE
    | ERROR
    | INFO
    | INVITE
    | ISON
    | JOIN
    | KICK
    | KILL
    | LINKS
    | LIST
    | LUSERS
    | MODE
    | MOTD
    | NAMES
    | NICK
    | NJOIN
    | NOTICE
    | OPER
    | PART
    | PASS
    | PING
    | PONG
    | PRIVMSG
    | QUIT
    | REHASH
    | RESTART
    | SERVER
    | SERVICE
    | SERVLIST
    | SQUERY
    | SQUIRT
    | SQUIT
    | STATS
    | SUMMON
    | TIME
    | TOPIC
    | TRACE
    | USER
    | USERHOST
    | USERS
    | VERSION
    | WALLOPS
    | WHO
    | WHOIS
    | WHOWAS

    | RPL_WELCOME
    | RPL_YOURHOST
    | RPL_CREATED
    | RPL_MYINFO
    | RPL_BOUNCE

    | RPL_TRACELINK
    | RPL_TRACECONNECTING
    | RPL_TRACEHANDSHAKE
    | RPL_TRACEUNKNOWN
    | RPL_TRACEOPERATOR
    | RPL_TRACEUSER
    | RPL_TRACESERVER
    | RPL_TRACESERVICE
    | RPL_TRACENEWTYPE
    | RPL_TRACECLASS
    | RPL_TRACERECONNECT
    | RPL_STATSLINKINFO
    | RPL_STATSCOMMANDS
    | RPL_ENDOFSTATS
    | RPL_UMODEIS
    | RPL_SERVLIST
    | RPL_SERVLISTEND
    | RPL_STATSUPTIME
    | RPL_STATSOLINE
    | RPL_LUSERCLIENT
    | RPL_LUSEROP
    | RPL_LUSERUNKNOWN
    | RPL_LUSERCHANNELS
    | RPL_LUSERME
    | RPL_ADMINME
    | RPL_ADMINLOC1
    | RPL_ADMINLOC2
    | RPL_ADMINEMAIL
    | RPL_TRACELOG
    | RPL_TRACEEND
    | RPL_TRYAGAIN
    | RPL_AWAY
    | RPL_USERHOST
    | RPL_ISON
    | RPL_UNAWAY
    | RPL_NOWAWAY
    | RPL_WHOISUSER
    | RPL_WHOISSERVER
    | RPL_WHOISOPERATOR
    | RPL_WHOWASUSER
    | RPL_ENDOFWHO
    | RPL_WHOISIDLE
    | RPL_ENDOFWHOIS
    | RPL_WHOISCHANNELS
    | RPL_LISTSTART
    | RPL_LIST
    | RPL_LISTEND
    | RPL_CHANNELMODEIS
    | RPL_UNIQOPIS
    | RPL_NOTOPIC
    | RPL_TOPIC
    | RPL_INVITING
    | RPL_SUMMONING
    | RPL_INVITELIST
    | RPL_ENDOFINVITELIST
    | RPL_EXCEPTLIST
    | RPL_ENDOFEXCEPTLIST
    | RPL_VERSION
    | RPL_WHOREPLY
    | RPL_NAMREPLY
    | RPL_LINKS
    | RPL_ENDOFLINKS
    | RPL_ENDOFNAMES
    | RPL_BANLIST
    | RPL_ENDOFBANLIST
    | RPL_ENDOFWHOWAS
    | RPL_INFO
    | RPL_MOTD
    | RPL_ENDOFINFO
    | RPL_MOTDSTART
    | RPL_ENDOFMOTD
    | RPL_YOUREOPER
    | RPL_REHASHING
    | RPL_YOURESERVICE
    | RPL_TIME
    | RPL_USERSSTART
    | RPL_USERS
    | RPL_ENDOFUSERS
    | RPL_NOUSERS

    | ERR_NOSUCHNICK
    | ERR_NOSUCHSERVER
    | ERR_NOSUCHCHANNEL
    | ERR_CANNOTSENDTOCHAN
    | ERR_TOOMANYCHANNELS
    | ERR_WASNOSUCHNICK
    | ERR_TOOMANYTARGETS
    | ERR_NOSUCHSERVICE
    | ERR_NOORIGIN
    | ERR_NORECIPIENT
    | ERR_NOTEXTTOSEND
    | ERR_NOTOPLEVEL
    | ERR_WILDTOPLEVEL
    | ERR_BADMASK
    | ERR_UNKNOWNCOMMAND
    | ERR_NOMOTD
    | ERR_NOADMININFO
    | ERR_FILEERROR
    | ERR_NONICKNAMEGIVEN
    | ERR_ERRONEUSNICKNAME
    | ERR_NICKNAMEINUSE
    | ERR_NICKCOLLISION
    | ERR_UNAVAILRESOURCE
    | ERR_USERNOTINCHANNEL
    | ERR_NOTONCHANNEL
    | ERR_USERONCHANNEL
    | ERR_NOLOGIN
    | ERR_SUMMONDISABLED
    | ERR_USERSDISABLED
    | ERR_NOTREGISTERED
    | ERR_NEEDMOREPARAMS
    | ERR_ALREADYREGISTRED
    | ERR_NOPERMFORHOST
    | ERR_PASSWDMISMATCH
    | ERR_YOUREBANNEDCREEP
    | ERR_YOUWILLBEBANNED
    | ERR_KEYSET
    | ERR_CHANNELISFULL
    | ERR_UNKNOWNMODE
    | ERR_INVITEONLYCHAN
    | ERR_BANNEDFROMCHAN
    | ERR_BADCHANNELKEY
    | ERR_BADCHANMASK
    | ERR_NOCHANMODES
    | ERR_BANLISTFULL
    | ERR_NOPRIVILEGES
    | ERR_CHANOPRIVSNEEDED
    | ERR_CANTKILLSERVER
    | ERR_RESTRICTED
    | ERR_UNIQOPPRIVSNEEDED
    | ERR_NOOPERHOST
    | ERR_UMODEUNKNOWNFLAG
    | ERR_USERSDONTMATCH

    | RPL_LOGGEDIN
    | RPL_LOGGEDOUT
    | ERR_NICKLOCKED
    | RPL_SASLSUCCESS
    | ERR_SASLFAIL
    | ERR_SASLTOOLONG
    | ERR_SASLABORTED
    | ERR_SASLALREADY
  deriving (Eq,Generic,Hashable,Show)

instance ConvertibleStrings Text Command where
  convertString = convert
    where
      convert s = M.lookupDefault (fallback s) s mTextCommand
      fallback s =
        case T.decimal s of
          Right (i, "") -> UnknownReply i
          _ -> UnknownCommand s

instance ConvertibleStrings Command Text where
  convertString = convert
    where
      convert c = M.lookupDefault (fallback c) c mCommandText
      fallback = \case
          UnknownCommand c -> c
          UnknownReply i -> show3 i
          x -> error ("no fallback for " <> show x)

instance FromJSON Command where
  parseJSON = \case
    String t -> pure (convertString t)
    invalid -> typeMismatch "Command" invalid

instance FromJSONKey Command where
  fromJSONKey = FromJSONKeyText convertString


commands :: [(Text, Command)]
commands =
    [ ("ADMIN", ADMIN)
    , ("AUTHENTICATE", AUTHENTICATE)
    , ("AWAY", AWAY)
    , ("CAP", CAP)
    , ("CONNECT", CONNECT)
    , ("DIE", DIE)
    , ("ERROR", ERROR)
    , ("INFO", INFO)
    , ("INVITE", INVITE)
    , ("ISON", ISON)
    , ("JOIN", JOIN)
    , ("KICK", KICK)
    , ("KILL", KILL)
    , ("LINKS", LINKS)
    , ("LIST", LIST)
    , ("LUSERS", LUSERS)
    , ("MODE", MODE)
    , ("MOTD", MOTD)
    , ("NAMES", NAMES)
    , ("NICK", NICK)
    , ("NJOIN", NJOIN)
    , ("NOTICE", NOTICE)
    , ("OPER", OPER)
    , ("PART", PART)
    , ("PASS", PASS)
    , ("PING", PING)
    , ("PONG", PONG)
    , ("PRIVMSG", PRIVMSG)
    , ("QUIT", QUIT)
    , ("REHASH", REHASH)
    , ("RESTART", RESTART)
    , ("SERVER", SERVER)
    , ("SERVICE", SERVICE)
    , ("SERVLIST", SERVLIST)
    , ("SQUERY", SQUERY)
    , ("SQUIRT", SQUIRT)
    , ("SQUIT", SQUIT)
    , ("STATS", STATS)
    , ("SUMMON", SUMMON)
    , ("TIME", TIME)
    , ("TOPIC", TOPIC)
    , ("TRACE", TRACE)
    , ("USER", USER)
    , ("USERHOST", USERHOST)
    , ("USERS", USERS)
    , ("VERSION", VERSION)
    , ("WALLOPS", WALLOPS)
    , ("WHO", WHO)
    , ("WHOIS", WHOIS)
    , ("WHOWAS", WHOWAS)
    ]

replies :: [(Int, Command)]
replies =
    [ (001, RPL_WELCOME)
    , (002, RPL_YOURHOST)
    , (003, RPL_CREATED)
    , (004, RPL_MYINFO)
    , (005, RPL_BOUNCE)

    , (200, RPL_TRACELINK)
    , (201, RPL_TRACECONNECTING)
    , (202, RPL_TRACEHANDSHAKE)
    , (203, RPL_TRACEUNKNOWN)
    , (204, RPL_TRACEOPERATOR)
    , (205, RPL_TRACEUSER)
    , (206, RPL_TRACESERVER)
    , (207, RPL_TRACESERVICE)
    , (208, RPL_TRACENEWTYPE)
    , (209, RPL_TRACECLASS)
    , (210, RPL_TRACERECONNECT)
    , (211, RPL_STATSLINKINFO)
    , (212, RPL_STATSCOMMANDS)
    , (219, RPL_ENDOFSTATS)
    , (221, RPL_UMODEIS)
    , (234, RPL_SERVLIST)
    , (235, RPL_SERVLISTEND)
    , (242, RPL_STATSUPTIME)
    , (243, RPL_STATSOLINE)
    , (251, RPL_LUSERCLIENT)
    , (252, RPL_LUSEROP)
    , (253, RPL_LUSERUNKNOWN)
    , (254, RPL_LUSERCHANNELS)
    , (255, RPL_LUSERME)
    , (256, RPL_ADMINME)
    , (257, RPL_ADMINLOC1)
    , (258, RPL_ADMINLOC2)
    , (259, RPL_ADMINEMAIL)
    , (261, RPL_TRACELOG)
    , (262, RPL_TRACEEND)
    , (263, RPL_TRYAGAIN)
    , (301, RPL_AWAY)
    , (302, RPL_USERHOST)
    , (303, RPL_ISON)
    , (305, RPL_UNAWAY)
    , (306, RPL_NOWAWAY)
    , (311, RPL_WHOISUSER)
    , (312, RPL_WHOISSERVER)
    , (313, RPL_WHOISOPERATOR)
    , (314, RPL_WHOWASUSER)
    , (315, RPL_ENDOFWHO)
    , (317, RPL_WHOISIDLE)
    , (318, RPL_ENDOFWHOIS)
    , (319, RPL_WHOISCHANNELS)
    , (321, RPL_LISTSTART)
    , (322, RPL_LIST)
    , (323, RPL_LISTEND)
    , (324, RPL_CHANNELMODEIS)
    , (325, RPL_UNIQOPIS)
    , (331, RPL_NOTOPIC)
    , (332, RPL_TOPIC)
    , (341, RPL_INVITING)
    , (342, RPL_SUMMONING)
    , (346, RPL_INVITELIST)
    , (347, RPL_ENDOFINVITELIST)
    , (348, RPL_EXCEPTLIST)
    , (349, RPL_ENDOFEXCEPTLIST)
    , (351, RPL_VERSION)
    , (352, RPL_WHOREPLY)
    , (353, RPL_NAMREPLY)
    , (364, RPL_LINKS)
    , (365, RPL_ENDOFLINKS)
    , (366, RPL_ENDOFNAMES)
    , (367, RPL_BANLIST)
    , (368, RPL_ENDOFBANLIST)
    , (369, RPL_ENDOFWHOWAS)
    , (371, RPL_INFO)
    , (372, RPL_MOTD)
    , (374, RPL_ENDOFINFO)
    , (375, RPL_MOTDSTART)
    , (376, RPL_ENDOFMOTD)
    , (381, RPL_YOUREOPER)
    , (382, RPL_REHASHING)
    , (383, RPL_YOURESERVICE)
    , (391, RPL_TIME)
    , (392, RPL_USERSSTART)
    , (393, RPL_USERS)
    , (394, RPL_ENDOFUSERS)
    , (395, RPL_NOUSERS)

    , (401, ERR_NOSUCHNICK)
    , (402, ERR_NOSUCHSERVER)
    , (403, ERR_NOSUCHCHANNEL)
    , (404, ERR_CANNOTSENDTOCHAN)
    , (405, ERR_TOOMANYCHANNELS)
    , (406, ERR_WASNOSUCHNICK)
    , (407, ERR_TOOMANYTARGETS)
    , (408, ERR_NOSUCHSERVICE)
    , (409, ERR_NOORIGIN)
    , (411, ERR_NORECIPIENT)
    , (412, ERR_NOTEXTTOSEND)
    , (413, ERR_NOTOPLEVEL)
    , (414, ERR_WILDTOPLEVEL)
    , (415, ERR_BADMASK)
    , (421, ERR_UNKNOWNCOMMAND)
    , (422, ERR_NOMOTD)
    , (423, ERR_NOADMININFO)
    , (424, ERR_FILEERROR)
    , (431, ERR_NONICKNAMEGIVEN)
    , (432, ERR_ERRONEUSNICKNAME)
    , (433, ERR_NICKNAMEINUSE)
    , (436, ERR_NICKCOLLISION)
    , (437, ERR_UNAVAILRESOURCE)
    , (441, ERR_USERNOTINCHANNEL)
    , (442, ERR_NOTONCHANNEL)
    , (443, ERR_USERONCHANNEL)
    , (444, ERR_NOLOGIN)
    , (445, ERR_SUMMONDISABLED)
    , (446, ERR_USERSDISABLED)
    , (451, ERR_NOTREGISTERED)
    , (461, ERR_NEEDMOREPARAMS)
    , (462, ERR_ALREADYREGISTRED)
    , (463, ERR_NOPERMFORHOST)
    , (464, ERR_PASSWDMISMATCH)
    , (465, ERR_YOUREBANNEDCREEP)
    , (466, ERR_YOUWILLBEBANNED)
    , (467, ERR_KEYSET)
    , (471, ERR_CHANNELISFULL)
    , (472, ERR_UNKNOWNMODE)
    , (473, ERR_INVITEONLYCHAN)
    , (474, ERR_BANNEDFROMCHAN)
    , (475, ERR_BADCHANNELKEY)
    , (476, ERR_BADCHANMASK)
    , (477, ERR_NOCHANMODES)
    , (478, ERR_BANLISTFULL)
    , (481, ERR_NOPRIVILEGES)
    , (482, ERR_CHANOPRIVSNEEDED)
    , (483, ERR_CANTKILLSERVER)
    , (484, ERR_RESTRICTED)
    , (485, ERR_UNIQOPPRIVSNEEDED)
    , (491, ERR_NOOPERHOST)
    , (501, ERR_UMODEUNKNOWNFLAG)
    , (502, ERR_USERSDONTMATCH)

    , (900, RPL_LOGGEDIN)
    , (901, RPL_LOGGEDOUT)
    , (902, ERR_NICKLOCKED)
    , (903, RPL_SASLSUCCESS)
    , (904, ERR_SASLFAIL)
    , (905, ERR_SASLTOOLONG)
    , (906, ERR_SASLABORTED)
    , (907, ERR_SASLALREADY)
    ]

mCommandText :: HashMap Command Text
mCommandText =
    M.fromList $
      map (\(s,c) -> (c,s)) commands <>
      map (\(i,c) -> (c,show3 i)) replies

mTextCommand :: HashMap Text Command
mTextCommand =
    M.fromList $
      map (\(s,c) -> (s,c)) commands <>
      map (\(i,c) -> (show3 i,c)) replies

show3 :: Int -> Text
show3 i =
    p <> s
  where s = T.show i
        p = T.replicate (3 - T.length s) "0"
