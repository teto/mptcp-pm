{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "/home/teto/iproute2/include/netinet/tcp.h"
module Toto.Test.Tcp where
import Foreign.Ptr
#strict_import

{- struct tcphdr {
    u_int16_t source;
    u_int16_t dest;
    u_int32_t seq;
    u_int32_t ack_seq;
    u_int16_t res1 : 4;
    u_int16_t doff : 4;
    u_int16_t fin : 1;
    u_int16_t syn : 1;
    u_int16_t rst : 1;
    u_int16_t psh : 1;
    u_int16_t ack : 1;
    u_int16_t urg : 1;
    u_int16_t res2 : 2;
    u_int16_t window;
    u_int16_t check;
    u_int16_t urg_ptr;
}; -}
#starttype struct tcphdr
#field source , CUInt
#field dest , CUInt
#field seq , CUInt
#field ack_seq , CUInt
#field res1 , CUInt
#field doff , CUInt
#field fin , CUInt
#field syn , CUInt
#field rst , CUInt
#field psh , CUInt
#field ack , CUInt
#field urg , CUInt
#field res2 , CUInt
#field window , CUInt
#field check , CUInt
#field urg_ptr , CUInt
#stoptype
{- enum {
    TCP_ESTABLISHED = 1,
    TCP_SYN_SENT,
    TCP_SYN_RECV,
    TCP_FIN_WAIT1,
    TCP_FIN_WAIT2,
    TCP_TIME_WAIT,
    TCP_CLOSE,
    TCP_CLOSE_WAIT,
    TCP_LAST_ACK,
    TCP_LISTEN,
    TCP_CLOSING
}; -}
#num TCP_ESTABLISHED
#num TCP_SYN_SENT
#num TCP_SYN_RECV
#num TCP_FIN_WAIT1
#num TCP_FIN_WAIT2
#num TCP_TIME_WAIT
#num TCP_CLOSE
#num TCP_CLOSE_WAIT
#num TCP_LAST_ACK
#num TCP_LISTEN
#num TCP_CLOSING
{- enum tcp_ca_state {
    TCP_CA_Open = 0,
    TCP_CA_Disorder = 1,
    TCP_CA_CWR = 2,
    TCP_CA_Recovery = 3,
    TCP_CA_Loss = 4
}; -}
#integral_t enum tcp_ca_state
#num TCP_CA_Open
#num TCP_CA_Disorder
#num TCP_CA_CWR
#num TCP_CA_Recovery
#num TCP_CA_Loss
{- struct tcp_info {
    u_int8_t tcpi_state;
    u_int8_t tcpi_ca_state;
    u_int8_t tcpi_retransmits;
    u_int8_t tcpi_probes;
    u_int8_t tcpi_backoff;
    u_int8_t tcpi_options;
    u_int8_t tcpi_snd_wscale : 4, tcpi_rcv_wscale : 4;
    u_int32_t tcpi_rto;
    u_int32_t tcpi_ato;
    u_int32_t tcpi_snd_mss;
    u_int32_t tcpi_rcv_mss;
    u_int32_t tcpi_unacked;
    u_int32_t tcpi_sacked;
    u_int32_t tcpi_lost;
    u_int32_t tcpi_retrans;
    u_int32_t tcpi_fackets;
    u_int32_t tcpi_last_data_sent;
    u_int32_t tcpi_last_ack_sent;
    u_int32_t tcpi_last_data_recv;
    u_int32_t tcpi_last_ack_recv;
    u_int32_t tcpi_pmtu;
    u_int32_t tcpi_rcv_ssthresh;
    u_int32_t tcpi_rtt;
    u_int32_t tcpi_rttvar;
    u_int32_t tcpi_snd_ssthresh;
    u_int32_t tcpi_snd_cwnd;
    u_int32_t tcpi_advmss;
    u_int32_t tcpi_reordering;
    u_int32_t tcpi_rcv_rtt;
    u_int32_t tcpi_rcv_space;
    u_int32_t tcpi_total_retrans;
}; -}
#starttype struct tcp_info
#field tcpi_state , CUInt
#field tcpi_ca_state , CUInt
#field tcpi_retransmits , CUInt
#field tcpi_probes , CUInt
#field tcpi_backoff , CUInt
#field tcpi_options , CUInt
#field tcpi_snd_wscale , CUInt
#field tcpi_rcv_wscale , CUInt
#field tcpi_rto , CUInt
#field tcpi_ato , CUInt
#field tcpi_snd_mss , CUInt
#field tcpi_rcv_mss , CUInt
#field tcpi_unacked , CUInt
#field tcpi_sacked , CUInt
#field tcpi_lost , CUInt
#field tcpi_retrans , CUInt
#field tcpi_fackets , CUInt
#field tcpi_last_data_sent , CUInt
#field tcpi_last_ack_sent , CUInt
#field tcpi_last_data_recv , CUInt
#field tcpi_last_ack_recv , CUInt
#field tcpi_pmtu , CUInt
#field tcpi_rcv_ssthresh , CUInt
#field tcpi_rtt , CUInt
#field tcpi_rttvar , CUInt
#field tcpi_snd_ssthresh , CUInt
#field tcpi_snd_cwnd , CUInt
#field tcpi_advmss , CUInt
#field tcpi_reordering , CUInt
#field tcpi_rcv_rtt , CUInt
#field tcpi_rcv_space , CUInt
#field tcpi_total_retrans , CUInt
#stoptype
