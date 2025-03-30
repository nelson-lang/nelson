#!/usr/bin/env just --justfile

os := if `uname` == "Darwin" { "macOS" } else { `uname | tr '[:upper:]' '[:lower:]'` }

default:
    @just --list

config:
    cmake -G "Unix Makefiles" .

build:
    cmake --build . -- -j$(nproc)

build_help:
    @echo "Building helps..."
    cmake --build . -- buildhelp

tests:
    @echo "Running tests..."
    ./bin/{{os}}/nelson --adv-cli -noipc --quiet -f ./tools/tests_all/runtests_all.m

minimal_tests:
    @echo "Running minimal tests..."
    cmake --build . -- tests_minimal

benchs:
    @echo "Running benchmarks..."
    cmake --build . -- benchs_all

start:
    ./bin/{{os}}/nelson
