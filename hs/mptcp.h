/*
 * Netlink API for Multipath TCP
 *
 * Author: Gregory Detal <gregory.detal@tessares.net>
 *
 *	This program is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either version
 *	2 of the License, or (at your option) any later version.
 */

#ifndef _LINUX_MPTCP_H
#define _LINUX_MPTCP_H

#define MPTCP_GENL_NAME		"mptcp"
#define MPTCP_GENL_EV_GRP_NAME	"mptcp_events"
#define MPTCP_GENL_CMD_GRP_NAME "mptcp_commands"
#define MPTCP_GENL_VER		0x1

/*
 * ATTR types defined for MPTCP
 */
enum {
	MPTCP_ATTR_UNSPEC = 0,

	MPTCP_ATTR_TOKEN,	/* u32 */
	MPTCP_ATTR_FAMILY,	/* u16 */
	MPTCP_ATTR_LOC_ID,	/* u8 */
	MPTCP_ATTR_REM_ID,	/* u8 */
	MPTCP_ATTR_SADDR4,	/* u32 */
	MPTCP_ATTR_SADDR6,	/* struct in6_addr */
	MPTCP_ATTR_DADDR4,	/* u32 */
	MPTCP_ATTR_DADDR6,	/* struct in6_addr */
	MPTCP_ATTR_SPORT,	/* u16 */
	MPTCP_ATTR_DPORT,	/* u16 */
	MPTCP_ATTR_BACKUP,	/* u8 */
	MPTCP_ATTR_ERROR,	/* u8 */
	MPTCP_ATTR_FLAGS,	/* u16 */
	MPTCP_ATTR_TIMEOUT,	/* u32 */
	MPTCP_ATTR_IF_IDX,	/* s32 */

	__MPTCP_ATTR_AFTER_LAST
};

#define MPTCP_ATTR_MAX (__MPTCP_ATTR_AFTER_LAST - 1)

/*
 * Events generated by MPTCP:
 *   - MPTCP_EVENT_CREATED: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                          sport, dport
 *       A new connection has been created.
 *
 *   - MPTCP_EVENT_ESTABLISHED: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                              sport, dport
 *       A connection is established (can start new subflows).
 *
 *   - MPTCP_EVENT_CLOSED: token
 *       A connection has stopped.
 *
 *   - MPTCP_EVENT_ANNOUNCED: token, rem_id, family, daddr4 | daddr6 [, dport]
 *       A new address has been announced by the peer.
 *
 *   - MPTCP_EVENT_REMOVED: token, rem_id
 *       An address has been lost by the peer.
 *
 *   - MPTCP_EVENT_SUB_CREATED: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                              sport, dport, backup, if_idx [, error]
 *       A new subflow has been created. 'error' should not be set.
 *
 *   - MPTCP_EVENT_SUB_ESTABLISHED: token, family, saddr4 | saddr6,
 *                                  daddr4 | daddr6, sport, dport, backup,
 *                                  if_idx [, error]
 *       A new subflow has been established. 'error' should not be set.
 *
 *   - MPTCP_EVENT_SUB_CLOSED: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                             sport, dport, backup, if_idx [, error]
 *       A subflow has been closed. An error (copy of sk_error) can be set.
 *
 *   - MPTCP_EVENT_SUB_PRIORITY: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                               sport, dport, backup, if_idx [, error]
 *       The priority of a subflow has changed. 'error' should not be set.
 *
 *   - MPTCP_EVENT_SUB_ERROR: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                            sport, dport, backup, if_idx, error
 *       A subflow got an error. Note that in case of error
 *       'MPTCP_EVENT_SUB_CLOSED' event will also be produced with an error.
 *
 * Commands for MPTCP:
 *   - MPTCP_CMD_ANNOUNCE: token, loc_id, family, saddr4 | saddr6 [, sport]
 *       Announce a new address to the peer.
 *
 *   - MPTCP_CMD_REMOVE: token, loc_id
 *       Announce that an address has been lost to the peer.
 *
 *   - MPTCP_CMD_SUB_CREATE: token, family, loc_id, rem_id, [saddr4 | saddr6,
 *                           daddr4 | daddr6, dport [, sport, backup, if_idx]]
 *       Create a new subflow.
 *
 *   - MPTCP_CMD_SUB_DESTROY: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                            sport, dport
 *       Close a subflow.
 *
 *   - MPTCP_CMD_SUB_PRIORITY: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                             sport, dport, backup
 *       Change the priority of a subflow.
 *
 *   - MPTCP_CMD_RESET: token,
 *       Reset the connection.
 *
 *   - MPTCP_CMD_SET_FILTER: flags
 *       Set the filter on events. Set MPTCPF_* flags to only receive specific
 *       events. Default is to receive all events.
 *
 *   - MPTCP_CMD_SUB_TIMEOUT: token, family, saddr4 | saddr6, daddr4 | daddr6,
 *                             sport, dport, timeout
 *       Change the timeout (TCP_USER_TIMEOUT) used for a subflow.
 *
 *   - MPTCP_CMD_DUMP: None
 *       Dump the state of the kernel. Only the DUMP functionality is accepted.
 *       This will generate Netlink messages for the following events:
 *         * MPTCP_EVENT_CREATED
 *         * MPTCP_EVENT_SUB_CREATED
 *
 *   - MPTCP_CMD_EXIST: token
 *       Check if this token is linked to an existing socket.
 */
enum {
	MPTCP_CMD_UNSPEC = 0,

	MPTCP_EVENT_CREATED,
	MPTCP_EVENT_ESTABLISHED,
	MPTCP_EVENT_CLOSED,

	MPTCP_CMD_ANNOUNCE,
	MPTCP_CMD_REMOVE,
	MPTCP_EVENT_ANNOUNCED,
	MPTCP_EVENT_REMOVED,

	MPTCP_CMD_SUB_CREATE,
	MPTCP_CMD_SUB_DESTROY,
	MPTCP_EVENT_SUB_CREATED,
	MPTCP_EVENT_SUB_ESTABLISHED,
	MPTCP_EVENT_SUB_CLOSED,

	MPTCP_CMD_SUB_PRIORITY,
	MPTCP_EVENT_SUB_PRIORITY,

	MPTCP_CMD_RESET,

	MPTCP_CMD_SET_FILTER,

	MPTCP_CMD_SUB_TIMEOUT,

	MPTCP_CMD_DUMP,

	MPTCP_CMD_EXIST,

	MPTCP_EVENT_SUB_ERROR,

	__MPTCP_CMD_AFTER_LAST
};

#define MPTCP_CMD_MAX (__MPTCP_CMD_AFTER_LAST - 1)

enum {
	MPTCPF_EVENT_CREATED		= (1 << 1),
	MPTCPF_EVENT_ESTABLISHED	= (1 << 2),
	MPTCPF_EVENT_CLOSED		= (1 << 3),
	MPTCPF_EVENT_ANNOUNCED		= (1 << 4),
	MPTCPF_EVENT_REMOVED		= (1 << 5),
	MPTCPF_EVENT_SUB_CREATED	= (1 << 6),
	MPTCPF_EVENT_SUB_ESTABLISHED	= (1 << 7),
	MPTCPF_EVENT_SUB_CLOSED		= (1 << 8),
	MPTCPF_EVENT_SUB_PRIORITY	= (1 << 9),
	MPTCPF_EVENT_SUB_ERROR		= (1 << 10),
};

#endif /* _LINUX_MPTCP_H */
