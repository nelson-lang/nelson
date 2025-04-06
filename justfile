#!/usr/bin/env just --justfile

os := if `uname` == "Darwin" { "macOS" } else { `uname | tr '[:upper:]' '[:lower:]'` }

default:
    @just --list

clean:
    @echo "Clean all..."
    cmake --build . --target clean
    rm -rf CMakeFiles CMakeCache.txt Makefile cmake_install.cmake
    rm -rf node_modules

config:
    @echo "Configure..."
    cmake -G "Unix Makefiles" .

build:
    @echo "Building..."
    cmake --build . -- -j$(nproc)

build_help:
    @echo "Building helps..."
    cmake --build . -- buildhelp

get_module_skeleteton: build
    @echo "Get module skeleton"
    cmake --build . -- get_module_skeleton

tests:
    @echo "Running tests..."
    ./bin/{{os}}/nelson --adv-cli -noipc --quiet -f ./tools/tests_all/runtests_all.m

minimal_tests:
    @echo "Running minimal tests..."
    cmake --build . -- tests_minimal

benchs:
    @echo "Running benchmarks..."
    cmake --build . -- benchs_all

prettier:
    @echo "Prettier"
    npm run prettier

start:
    ./bin/{{os}}/nelson
