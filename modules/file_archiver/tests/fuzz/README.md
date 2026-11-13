# ZIP reader fuzzing

Two libFuzzer targets cover the code that reads untrusted ZIP data:

- `zip_central_directory_fuzzer`: the central directory reader (`ZipCentralDirectory.cpp`).
- `zip_archive_fuzzer`: `ZipArchive::openMemory` then every entry read, with and without a password.

`corpus/zip` holds the seed archives and the inputs that once failed (`regression_*`).

## Replay the corpus (any compiler)

```sh
cmake -S modules/file_archiver/tests/fuzz -B build/fuzz-replay
cmake --build build/fuzz-replay
ctest --test-dir build/fuzz-replay --output-on-failure
```

## Fuzz (clang, or MSVC with `/fsanitize=fuzzer`)

```sh
cmake -S modules/file_archiver/tests/fuzz -B build/fuzz -DNELSON_FUZZ=ON \
  -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
cmake --build build/fuzz
cp -r modules/file_archiver/tests/fuzz/corpus/zip corpus-work
./build/fuzz/zip_archive_fuzzer corpus-work -max_total_time=300 -timeout=10 -rss_limit_mb=2048
```

A failing input is written as `crash-*`, `oom-*` or `timeout-*`: fix the code, then copy the
input to `corpus/zip/regression_<what>.zip` so the replay test keeps it fixed.
