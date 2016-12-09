# A Distributed, Peer-to-Peer File Backup Service
This is a semester project for COMP 50: Concurrent Programming,
Fall 2016.
## Authors
Xuanrui Qi, Brinley Macnamara, and Benjamin Holen.

## Usage
* Before use, please modify `Makefile` and set `$LONGNAME` to the node
  name you desire. Create a file `ip.conf`, which contains the IP address
  of the monitor, if you desire to run the peer.
* Start peer: `make runpeer`
* Start monitor: `make runmonitor`

## Overview of Code
Code is be divided into four sections: client, server, monitor and utility.
Client and server both run on each peer in
the cluster, and monitor runs only on the monitor of that cluster:
### Client:  
  * start_client.erl: starts the client
  * client.erl: main functions for the client
### Server:  
  * tcp_server.erl: the TCP server
  * tcp_sup.erl: the TCP supervisor
### Monitor:  
  * start_monitor.erl: starts the monitor
  * monitor.erl: main functions for the monitor
  * monitor_tcp_server.erl: the TCP server for monitors
  * monitor_tcp_sup.erl: the TCP supervisor for monitors
### Utility:  
  * file_proc.erl: utility for file and file packets
  * database.erl: high level interface for the dets database

## License
MIT License. See "LICENSE" file.
