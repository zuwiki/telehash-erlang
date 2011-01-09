TeleHash-Erlang
===============

A TeleHash Switch implementation in Erlang
------------------------------------------

Let's try it out...

    $ erl -make
    $ erl -pa ebin/
    Erlang R14B (erts-5.8.1) ...
    Eshell V5.8.1  (abort with ^G)
    1> {ok, Switch} = telehash:start_link(["doggy", 5555]).
    {ok,<0.33.0>}
    2> rr("include/telehash.hrl").
    [switch,tap,telex]
    3> telehash:get_state(Switch).                         
    #switch{ipp = undefined,
            hash = <<"f03b0a8932f1e3cce41d0dc916e20d489194e1d1">>,
            socket = #Port<0.718>,lines = [],
            buckets = {[],[],[],[]},
            callbacks = []}

Now in a fresh terminal...

    $ echo this_is_me | shasum
    c674e99ad10377acf2731fdb9d9123b6b9d79f7d  -
    $ echo '{"+end":"c674e99ad10377acf2731fdb9d9123b6b9d79f7d","_to":"127.0.0.1:5555"}' | nc -u 127.0.0.1 5555
    # And you will get back...
    {"foo":"bar","rab":"oof"}

Back in the Erlang shell, you should see something like this...

    127.0.0.1:65296 sent: {"+end":"c674e99ad10377acf2731fdb9d9123b6b9d79f7d","_to":"127.0.0.1:5555"}
        Decoded json: {struct,[{<<"+end">>,
                                <<"c674e99ad10377acf2731fdb9d9123b6b9d79f7d">>},
                               {<<"_to">>,<<"127.0.0.1:5555">>}]}
        Sending {"foo":"bar","rab":"oof"} to 127.0.0.1:65296
    4> telehash:get_state(Switch).
    #switch{ipp = {{127,0,0,1},5555},
            hash = <<"f03b0a8932f1e3cce41d0dc916e20d489194e1d1">>,
            socket = #Port<0.718>,lines = [],
            buckets = {[],[],[],[]},
            callbacks = []}

Other information
-----------------

This project is being worked on as part of a computer science course at the University of Chicago. The project's page is [here](http://brick.cs.uchicago.edu/Courses/CMSC-16200/2011/pmwiki/pmwiki.php/Student/TeleHash), and more information about why that page exists is available under the Course Overview section [here](http://brick.cs.uchicago.edu/Courses/CMSC-16200/2011/).

* **First TODO item:** send back a `_to` so this can act as a seed.

Other undocumented features:

* Omit the `Port` or set it to 0 in `telehash:start_link/1` to have the OS assign a free UDP port
* Yup, that's it

Thanks to Mochi for [mochijson2.erl](https://github.com/mochi/mochiweb/blob/master/src/mochijson2.erl).
