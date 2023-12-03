Dotfiles
========

Dotfiles, scripts etc.


Remote VMs!
===========

- Requires patched quickemu to not bind only to localhost.
- Run the vm ``quickemu --vm <vm-config>.conf --display spice --viewer none``
- The port needs to be opened in the firewall ``sudo iptables -I INPUT -p tcp --dport <port> -j ACCEPT``

- To close the port ``sudo iptables -D INPUT <rulenum>``

.. tip::

   To list the configured rules::

      $ sudo iptables --list --line-numbers

**On the client**

- Connect ``remote-viewer spice://<ip-address>:<port>``

