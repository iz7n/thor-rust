#!/bin/sh
cargo run -- code.thor --log &&
llc -filetype=obj output.ll &&
gcc output.ll -o thor &&
./thor