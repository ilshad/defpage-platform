Build
-----

1. Copy `rebar` executable file into this (platform/) directory.

2. Create config file:

  $ make config

3. Edit options in config file "include/platform.hrl".

4. Build application:

  $ make

Run
---

  $ ./platform {start|stop|attach|console}


Testing:

  $ curl -i --data '{"update_meta": 1}' --header "Content-Type: application/json" http://localhost:9010/process
