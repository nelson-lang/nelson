
# Minimal tests for the Nelson
minimal_tests:
    cmake --build . -- tests_minimal

# Tests for the Nelson
tests:
    ./bin/{{os}}/nelson --adv-cli -noipc --quiet -f ./tools/tests_all/runtests_all.m

# Tests for the Nelson (no display)
tests_no_display:
    cmake --build . -- -j $(nproc) tests_all_no_display    
