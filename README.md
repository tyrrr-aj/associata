# Parallel Associative Graph Data Network

## Overview
This repository contains a reference implementation of the parallelized Associative Graph Data Network.
Implementation is written in Erlang, and benchmarking code is provided in Python (using [Pyrlang](https://github.com/Pyrlang/Pyrlang) for communication).


## Requirements
Running the 'engine' code in this repository requires Erlang\OTP 25.

Running benchmark code requires Pyrlang.


## Compilation

To compile the AGDN implementation, navigate to the `aas-engine` directory and execute the command:
    
    ./rebar3-custom/rebar3 compile

Notice that this solution uses a **custom wrapper** around the rebar3 building tool.


## Running benchmark

First, if neccessary, please adjust the path to the `aas.cmd` file (generated during compilationn) in the `pyassoc/associata.py` file.

Then, benchmark can be executed by navigating to `pyassoc` directory and runnning the `benchmark.py` script:

    python benchmark.py

Script accepts a single optional paramter, specifying the number of CPU cores that should be available to AGDN (defaults to 16):

    python benchmark.py 4

Other benchmark parameters - the number of VNGs, VNs, epsilon values, etc. - can be set in the `benchmark.py` file itself.
