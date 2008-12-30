-module(veracity_codes).

-export([code/1]).

code("PRIVMSG") -> privmsg;
code("NOTICE") -> notice;
code(001) -> rpl_welcome;
code(002) -> rpl_yourhost;
code(003) -> rpl_created;
code(004) -> rpl_myinfo;
code(005) -> rpl_bounce;
code(302) -> rpl_userhost;
code(303) -> rpl_ison;
code(301) -> rpl_away;
code(305) -> rpl_unaway;
code(306) -> rpl_nowaway;
code(311) -> rpl_whoisuser;
code(312) -> rpl_whoisserver;
code(313) -> rpl_whoisoperator;
code(317) -> rpl_whoisidle;
code(318) -> rpl_endofwhois;
code(319) -> rpl_whoischannels;
code(320) -> rpl_whoislogin;
code(314) -> rpl_whowasuser;
code(369) -> rpl_endofwhowas;
code(321) -> rpl_liststart;
code(322) -> rpl_list;
code(323) -> rpl_listend;
code(325) -> rpl_uniqopis;
code(324) -> rpl_channelmodeis;
code(331) -> rpl_notopic;
code(332) -> rpl_topic;
code(341) -> rpl_inviting;
code(342) -> rpl_summoning;
code(346) -> rpl_invitelist;
code(347) -> rpl_endofinvitelist;
code(348) -> rpl_exceptlist;
code(349) -> rpl_endofexceptlist;
code(351) -> rpl_version;
code(352) -> rpl_whoreply;
code(315) -> rpl_endofwho;
code(353) -> rpl_namreply;
code(366) -> rpl_endofnames;
code(364) -> rpl_links;
code(365) -> rpl_endoflinks;
code(367) -> rpl_banlist;
code(368) -> rpl_endofbanlist;
code(371) -> rpl_info;
code(374) -> rpl_endofinfo;
code(375) -> rpl_motdstart;
code(372) -> rpl_motd;
code(376) -> rpl_endofmotd;
code(381) -> rpl_youreoper;
code(382) -> rpl_rehashing;
code(383) -> rpl_youreservice;
code(391) -> rpl_time;
code(392) -> rpl_usersstart;
code(393) -> rpl_users;
code(394) -> rpl_endofusers;
code(395) -> rpl_nousers;
code(200) -> rpl_tracelink;
code(201) -> rpl_traceconnecting;
code(202) -> rpl_tracehandshake;
code(203) -> rpl_traceunknown;
code(204) -> rpl_traceoperator;
code(205) -> rpl_traceuser;
code(206) -> rpl_traceserver;
code(207) -> rpl_traceservice;
code(208) -> rpl_tracenewtype;
code(209) -> rpl_traceclass;
code(210) -> rpl_tracereconnect;
code(261) -> rpl_tracelog;
code(262) -> rpl_traceend;
code(211) -> rpl_statslinkinfo;
code(212) -> rpl_statscommands;
code(219) -> rpl_endofstats;
code(242) -> rpl_statsuptime;
code(243) -> rpl_statsoline;
code(221) -> rpl_umodeis;
code(234) -> rpl_servlist;
code(235) -> rpl_servlistend;
code(251) -> rpl_luserclient;
code(252) -> rpl_luserop;
code(253) -> rpl_luserunknown;
code(254) -> rpl_luserchannels;
code(255) -> rpl_luserme;
code(256) -> rpl_adminme;
code(257) -> rpl_adminloc1;
code(258) -> rpl_adminloc2;
code(259) -> rpl_adminemail;
code(263) -> rpl_tryagain;
code(401) -> err_nosuchnick;
code(402) -> err_nosuchserver;
code(403) -> err_nosuchchannel;
code(404) -> err_cannotsendtochan;
code(405) -> err_toomanychannels;
code(406) -> err_wasnosuchnick;
code(407) -> err_toomanytargets;
code(408) -> err_nosuchservice;
code(409) -> err_noorigin;
code(411) -> err_norecipient;
code(412) -> err_notexttosend;
code(413) -> err_notoplevel;
code(414) -> err_wildtoplevel;
code(415) -> err_badmask;
code(421) -> err_unknowncommand;
code(422) -> err_nomotd;
code(423) -> err_noadmininfo;
code(424) -> err_fileerror;
code(431) -> err_nonicknamegiven;
code(432) -> err_erroneusnickname;
code(433) -> err_nicknameinuse;
code(Other) -> Other.
