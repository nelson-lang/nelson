// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "RuntimeLog.hpp"
#include <cstring>
#include <fstream>
#include <iostream>

using namespace Nelson::RuntimeLogging;
using Nelson::ApplicationArchive::require;

namespace {
std::string
readText(const std::filesystem::path& path)
{
    std::ifstream input(path, std::ios::binary);
    return { std::istreambuf_iterator<char>(input), {} };
}

void
waitFor(const std::filesystem::path& path, const std::string& marker)
{
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(5);
    while (readText(path).find(marker) == std::string::npos) {
        require(std::chrono::steady_clock::now() < deadline, "Runtime log marker timeout.");
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
}

NativeHandle
streamHandle(bool error)
{
#ifdef _WIN32
    return GetStdHandle(error ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
#else
    return error ? STDERR_FILENO : STDOUT_FILENO;
#endif
}

void
checkPrefixes()
{
    for (const std::string character : { "\xc3\xa9", "\xe2\x82\xac", "\xf0\x9f\x98\x80" }) {
        const auto text = "prefix" + character;
        for (size_t length = 1; length < character.size(); ++length) {
            require(completeUtf8Prefix(text.data(), 6 + length) == 6,
                "Incomplete character was not retained.");
        }
        require(completeUtf8Prefix(text.data(), text.size()) == text.size(),
            "Complete character was retained.");
    }
    require(completeUtf8Prefix("", 0) == 0, "Invalid empty prefix.");
    for (const std::string bytes : { "ASCII", "\xff", "\x80\x80\x80\x80\x80", "\xe2?", "\xf5" }) {
        require(completeUtf8Prefix(bytes.data(), bytes.size()) == bytes.size(),
            "Malformed or ASCII output was withheld.");
    }
}

void
writeConcurrent(const std::filesystem::path& directory, const std::filesystem::path& self, int id)
{
    require(id >= 0 && id < 6, "Invalid writer identifier.");
    Log log;
    log.open(directory / "concurrent-runtime.log", self);
    log.startCapture();
    std::ofstream(directory / ("writer-ready-" + std::to_string(id))) << "READY";
    waitFor(directory / "writers-start", "GO");
    auto payload = [id](bool error) {
        const auto record = std::string(1, static_cast<char>((error ? 'a' : 'A') + id))
            + "\xc3\xa9\xe2\x82\xac\xf0\x9f\x98\x80\n";
        std::string bytes;
        bytes.reserve(record.size() * 10000);
        for (int n = 0; n < 10000; ++n) {
            bytes += record;
        }
        return bytes;
    };
    const auto output = payload(false), error = payload(true);
    bool errorWritten = false;
    std::thread worker(
        [&]() { errorWritten = writeAll(streamHandle(true), error.data(), error.size()); });
    const bool outputWritten = writeAll(streamHandle(false), output.data(), output.size());
    worker.join();
    require(outputWritten && errorWritten && log.finish(), "Concurrent writer failed.");
}

void
checkInterleaved(const std::filesystem::path& directory, const std::filesystem::path& self)
{
    const auto path = directory / "unicode-runtime.log";
    std::ofstream(path, std::ios::binary).close();
    Log log;
    log.open(path, self);
    log.startCapture();
    for (bool error : { false, true }) {
        const auto first = error ? "ERROR_PREFIX:" : "OUTPUT_PREFIX:";
        const auto other = error ? "OTHER_OUTPUT\n" : "OTHER_ERROR\n";
        const auto leading = std::string(first) + "\xe2";
        require(writeAll(streamHandle(error), leading.data(), leading.size()), "Write failed.");
        waitFor(path, first);
        require(writeAll(streamHandle(!error), other, std::strlen(other)), "Write failed.");
        waitFor(path, other);
        const std::string tail = "\x82\xac\n";
        require(writeAll(streamHandle(error), tail.data(), tail.size()), "Write failed.");
        waitFor(path, "\x82\xac\n");
    }
    require(log.finish(), "Unicode log capture failed.");
    const auto text = readText(path);
    const std::string character = "\xe2\x82\xac";
    const auto first = text.find(character);
    require(first != std::string::npos
            && text.find(character, first + character.size()) != std::string::npos,
        "Output streams split UTF-8 characters in the runtime log.");
    const auto prefix = text.find("OUTPUT_PREFIX:");
    require(prefix != std::string::npos, "Missing output prefix.");
    auto actual = text.substr(prefix);
    auto expected = std::string("OUTPUT_PREFIX:OTHER_ERROR\n") + character
        + "\nERROR_PREFIX:OTHER_OUTPUT\n" + character + "\n";
    std::sort(actual.begin(), actual.end());
    std::sort(expected.begin(), expected.end());
    require(actual == expected, "Output bytes were lost or duplicated.");

    const auto truncated = directory / "truncated-runtime.log";
    std::ofstream(truncated, std::ios::binary).close();
    log.open(truncated, self);
    log.startCapture();
    require(writeAll(streamHandle(false), "\xf0\x9f\x98", 3), "Write failed.");
    require(log.finish(), "Truncated output capture failed.");
    const auto raw = readText(truncated);
    require(raw.size() >= 3 && raw.substr(raw.size() - 3) == "\xf0\x9f\x98",
        "Truncated output was not preserved verbatim.");
}
}

int
main(int argc, char* argv[])
{
    try {
        require(argc == 2 || argc == 4, "Expected a test output directory and optional writer id.");
        const auto directory = std::filesystem::u8path(argv[1]);
        const auto self = std::filesystem::absolute(std::filesystem::u8path(argv[0]));
        if (argc == 4) {
            require(std::string(argv[2]) == "writer", "Unknown mode.");
            writeConcurrent(directory, self, std::stoi(argv[3]));
            return 0;
        }
        checkPrefixes();
        checkInterleaved(directory, self);
        std::cout << "RUNTIME_LOG_UNICODE_OK\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
