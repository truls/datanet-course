-record(peerlist, {verified = false,
		   expire = 0,
		   minwait = 0,
		   nonce = "",
		   peers = [],
		   whitelist = [],
		   n,
		   e}).
		      
-record(peer, {ip,
	       port,
	       datetime,
	       superpeer,
	       n,
	       e}).

-record(key, {n, e}).

