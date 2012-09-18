Example code for TCP port scanners in Erlang
============================================

Quickstart
==========

Checkout the source code of the project:

    $~ git clone git://github.com/asconix/erlang-scanner-examples.git

The project requires the Erlang build tool *rebar*. Install it by executing the following commands:

    $~ git clone git://github.com/basho/rebar.git
    $~ cd rebar
    $~ ./bootstrap
    $~ cp rebar ../erlang-crawler-examples/

After *rebar* has been installed, fetch the required dependency packages:

    $~ cd ../erlang-scanner-examples/
    $~ ./rebar get-deps
    
... and compile the Erlang sources (dependencies and individual code):

    $~ ./rebar compile
    
Note: you can always revert all changes and cleanup the application with rebar:

    $~ ./rebar clean 
    $~ rm -f erl_crash.dump
    $~ rm -f data/*

Run the Erlang shell:

    $~ ./run

... and the crawler you want play with:

*simple_scanner*

    1> simple_scanner:start().

