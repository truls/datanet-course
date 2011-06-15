-record(peerlist, {expire = 0,
		   minwait = 0,
		   nonce = "",
		   peers = [],
		   whitelist = []}).
		      
-record(peer, {ip,
	       port,
	       datetime,
	       superpeer}).
