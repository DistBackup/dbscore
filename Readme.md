# A Distributed, Peer-to-Peer File Backup Service
This is a semester project for COMP 50: Concurrent Programming,
Fall 2016.
## Authors
Xuanrui Qi, Brinley Macnamara, and Benjamin Holen.

## Usage
* ssh into 3 lab machines (at minimum, our app needs one monitor and two clients (peers)) 
* cd into our project directory
* Choose one machine to be the monitor, and find the public ip address of this machine with: 
dig +short myip.opendns.com @resolver1.opendns.com
* Open ip.conf and replace the existing IP address with your monitor’s IP address. Save and quit.
* Start an erlang shell on each lab machine with: 
erl -name your_node_name -setcookie your_cookie
* In one erlang shell, compile our code with: make:all().
* Start a monitor on the machine you’ve designated as the monitor with: start_monitor:start().
* Start clients on the other machines with: start_client:start().
* Press enter on the client machines for a list of public functions.


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
