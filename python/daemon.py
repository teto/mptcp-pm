#!/usr/bin/env nix-shell
#!nix-shell shell-netlink.nix -i python
import sys
import netlink.capi as nl
import netlink.genl.capi as genl
import traceback
import logging
import argparse
import struct
import socket
import subprocess
import signal
import binascii
from constants import Commands, MptcpAttr

logger = logging.getLogger( __name__ )
logger.setLevel( logging.DEBUG )
# logger= logging
# print ("handlers", logger.handlers )

handler = logging.StreamHandler()
#logging.FileHandler('hello.log')
handler.setLevel(logging.DEBUG)
logger.addHandler( handler )


MPTCP_GENL_EV_GRP_NAME = "mptcp_events"
MPTCP_GENL_CMD_GRP_NAME  = "mptcp_commands"
MPTCP_GENL_VERSION       = 1

MPTCP_FAMILY_NAME   = MPTCP_GENL_NAME = "mptcp"

logger.debug("Starting MPTCP PM DAEMON\n");
# print("level" , logging.getLevelName( logger.getEffectiveLevel() ) )


# command/event
# dispatcher = {
# }

# TODO
def sigint_handler(signum, frame):
    print( 'Stop pressing the CTRL+C!' )

class PathManager:

    def add_membership(self):
        # nl.nl_socket_drop_membership
        pass

    def __init__(self, simulate=None):
        self.done = 1;
        # by default, should be possible to override
        self.simulate       = simulate

        # TODO replace by Socket
        # allocates callback
        tx_cb = nl.nl_cb_alloc(nl.NL_CB_DEFAULT)

        #Clone an existing callback handle
        self.rx_cb = nl.nl_cb_clone(tx_cb)

        # allocates sockets
        self.sk = nl.nl_socket_alloc_cb(tx_cb)

        # set callback handers
        # last parameter represents arguments to pass
        logger.info("Setting callback functions")

        # nl.py_nl_cb_err(self.rx_cb, nl.NL_CB_CUSTOM, error_handler, self);
        # nl_cb_set( callback_set, type, kind, function,args )
        nl.py_nl_cb_set(self.rx_cb, nl.NL_CB_FINISH, nl.NL_CB_VERBOSE, finish_handler, self);
        nl.py_nl_cb_set(self.rx_cb, nl.NL_CB_ACK, nl.NL_CB_VERBOSE, ack_handler, self);
        nl.py_nl_cb_set(self.rx_cb, nl.NL_CB_VALID, nl.NL_CB_CUSTOM, msg_handler, self);
        # nl.py_nl_cb_set(self.rx_cb, nl.NL_CB_VALID, nl.NL_CB_CUSTOM, self.handle, None);

        # Notifications do not use sequence numbers, disable sequence number checking.
        #nl.nl_socket_disable_seq_check(self.sk);
        #nl.nl_socket_disable_auto_ack(self.sk);

        # establish connection
        genl.genl_connect(self.sk)
        self.family_id = genl.genl_ctrl_resolve(self.sk, MPTCP_FAMILY_NAME)

        # register to the multicast group
        # print( dir( sys.modules["netlink.genl.capi"]) )
        # print( dir( sys.modules["netlink.capi"]) )
        logger.info("family %s registered with number %d"%(MPTCP_FAMILY_NAME, self.family_id));


        for grp_name in [ MPTCP_GENL_EV_GRP_NAME, MPTCP_GENL_CMD_GRP_NAME ]:
            group_id = genl.genl_ctrl_resolve_grp (self.sk, MPTCP_FAMILY_NAME, grp_name);
            if group_id  < 0 :
                logger.error("Could not find group %s. Is the adequate module loaded ?" % grp_name)
                exit(1)

            logger.info("Group id found: %d" % group_id);
            logger.info("Using mapresolver %s" % self.mapresolver)
            ret = nl.nl_socket_add_membership(self.sk, group_id);

            if ret == 0:
                logger.info("Registration successful")
            else:
                logger.error("Could not register to group")
                exit(1)


        if self.simulate:
            logger.info("Simulation mode enabled %d"%self.simulate)
        else:
            logger.info("Real mode enabled")


    # send answer via netlink
    # send it into antoehr thread ?
    def monitor_connection(self, token, min_interval=300):
        """
        subscribe to a certain connection (token) to collect its metrics
        at regular interval.
        The daemon will then sort the paths

        # and ask for its updates in order to 
        interval in ms or number of times of smallest rtt ?
        """
        logger.info("Monitoring connection with token '%d' " % (token,))

        msg = nl.nlmsg_alloc()

        interval = min_interval # min()

        # returns void*
        genl.genlmsg_put(
            msg,
            0, # port
            0, # seq nb
            self.family_id, # family_id
            0, # length of user header
            0, # optional flags
            Commands.MPTCP_CMD_MONITOR, # cmd
            MPTCP_GENL_VERSION # version
        )

        nl.nla_put_u32(msg, MptcpAttr.MPTCP_ATTR_TOKEN, token );
        nl.nla_put_u32(msg, MptcpAttr.MPTCP_ATTR_TIMEOUT, interval);

        err = nl.nl_send_auto_complete(self.sk, msg);
        if err < 0:
            logger.error("Error while sending answer")
            nl.nlmsg_free(msg)
            return False

        nl.nlmsg_free(msg)
        return True


    def run(self):
        err = 0
        # cbd.done > 0 and not err < 0
        while True:
            # expects handle / cb configuration
            # see nl.c:965
            err = nl.nl_recvmsgs(self.sk, self.rx_cb)
            # err = nl.nl_recvmsgs_default(self.sk)
            if err < 0:
                logger.error( "Error for nl_recvmsgs: %d: %s"% (err, nl.nl_geterror(err)) )
                break;


    #def retrieve_number_of_rlocs(self,eid):

    #   print("retrieve_number_of_rlocs")
    #   # if in simulation mode, always return the same answer
    #   if self.simulate:
    #       logger.info("Simulation mode returning %d for eid %s"%(self.simulate, eid) )
    #       return self.simulate

    #   try:
    #       #number_of_rlocs=$(lig -m $mapresolver $eid 2>&1 | grep -c up)
    #       #PATH_TOWARDS_PROGRAM
    #       cmd= self.lig_program + " -m " + self.mapresolver + eid +" 2>&1" + "| grep -c up" 
    #       # args = [ self.lig_program,"-m", self.mapresolver, eid , "2>&1" ]
    #       output = subprocess.check_output( cmd , shell=True);
    #       print( "Result: ", output.decode() )
    #       return int( output.decode() );
    #   except  subprocess.CalledProcessError as e:
    #       logger.error("Could not retrieve the correct number of rlocs. Return code: %d"%e.returncode)
    #       return -1


    def handle(self, m):

        print("Hello world from ember function");
        logger.debug("Handle Msg from class")

        try:
            nlmsghdr = nl.nlmsg_hdr(m)
            print("nlmsghdr: flags:", nlmsghdr.nlmsg_flags , "seq:", nlmsghdr.nlmsg_seq )

            genlhdr = genl.genlmsg_hdr(nlmsghdr)
            if not genlhdr:
                logger.error("Could not get generic header")
                return nl.NL_STOP

            # TODO loop
            # TODO check which one maps to a RST event
            # TODO MPTCP_EVENT_SUB_CLOSED / MPTCP_EVENT_REMOVED / MPTCP_EVENT_CLOSED
            if genlhdr.cmd == Commands.MPTCP_EVENT_ESTABLISHED:
                logger.info(" received establisehd event")

                # attrs = None
                print("Message handler got called");
                # int uhl, int max,
                errorCode, attrs = genl.py_genlmsg_parse(
                    nlmsghdr, 
                    0, # will be returned as an attribute
                    # max number of attributes we support
                    # http://lists.infradead.org/pipermail/libnl/2013-July/001022.html
                    200, 
                    None
                    )

                if errorCode < 0:
                    logger.error("An error happened while parsing attributes")
                    return nl.NL_STOP;

                logger.info("Looking for ELA")
                if MptcpAttr.MPTCP_ATTR_TOKEN in attrs:
                    print ("hello", attrs[MptcpAttr.MPTCP_ATTR_TOKEN])
                    token   = nl.nla_get_u32(attrs[MptcpAttr.MPTCP_ATTR_TOKEN])
                    print ("token", token)

                    # print("Requested EID ",eid, " for token ",binascii.hexlify( token ))
                    # I => unsigned int
                    # packed_value = struct.pack('I', eid)
                    # addr = socket.inet_ntoa(packed_value)

                    nb = 1
                    # nb = self.retrieve_number_of_rlocs( addr )
                    if nb < 0:
                        logger.warning("An error happened while retrieveing nb of rlocs")
                        return nl.NL_STOP
                    else:
                        #nlmsghdr.nlmsg_seq + 1
                        self.send_rlocs_list_for_eid(  0, token, nb )
                        return nl.NL_SKIP

                else:
                    logger.error("Missing critical attribute in packet")

            else:
                logger.warning("Unhandled command %d"% genlhdr.cmd)

            # nlmsg_data returns void* so not usable straightaway

            # TODO need to retrieve command
            # print("e", err)

            return nl.NL_SKIP

        except Exception as e:
            # (t,v,tb) = sys.exc_info()
            # print( "test", v.message, e )
            # traceback.print_tb(tb)
            logging.exception("Error ")

            return nl.NL_SKIP




def error_handler(err, a):
    print("error handler")
    logger.error("Error handler called")
    # a.done = err.error
    return nl.NL_STOP

def finish_handler(m, arg):
    print("finish handler")
    logger.info("finish_handler called")
    return nl.NL_SKIP

def ack_handler(m, arg):
    print("ack handler")
    logger.info("ack handler called")
    # arg.done = 0
    return nl.NL_STOP


def msg_handler(m, arg):

    print("msg handler called")
    # print ( dir (arg) )
    arg.handle(m)
    # return nl.NL_OK
    return nl.NL_SKIP 



###############################
###############################
## TO TEST LIBNL (remove later)
###############################
# msg_handler = "hello world"
ack_handler = None

if __name__ == '__main__':

    signal.signal(signal.SIGINT, sigint_handler)

    # run tests
    parser = argparse.ArgumentParser(
            description='Daemon listeling for mptcp netlink requests'
            )

    # parser.add_argument('mapresolver', nargs="?", default="153.16.49.112", #DEFAULT_MAPRESOLVER, 
            # help="Choose")

    parser.add_argument('--simulate', dest="number_of_subflows", type=int)

    # subparsers    = parser.add_subparsers(dest="mode", help='sub-command help')
    # parser = subparsers.add_parser('daemon',help='daemon help')

    args = parser.parse_args( sys.argv[1:] )

    try:
        # could pass mr, or simulate mode
        daemon = PathManager(
            simulate=(args.number_of_subflows or None) 
        )

        daemon.run();

    except Exception as e:
    # (type, value, traceback)
        (t, v, tb) = sys.exc_info()
        print("hello world",v )
        traceback.print_tb(tb)

