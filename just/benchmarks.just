
# Benchmarks 
benchmarks:
    cmake --build . -- benchmark_all

# Benchmarks without display
benchmarks_no_display:
    cmake --build . -- -j $(nproc) benchmark_all_no_display




