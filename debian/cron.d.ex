#
# Regular cron jobs for the cpg-network package
#
0 4	* * *	root	[ -x /usr/bin/cpg-network_maintenance ] && /usr/bin/cpg-network_maintenance
