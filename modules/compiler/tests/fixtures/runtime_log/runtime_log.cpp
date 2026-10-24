// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "RuntimeLog.hpp"
#include "ApplicationStartupConfiguration.hpp"
#include <fstream>
#include <iostream>

using namespace Nelson::RuntimeLogging;
using Nelson::ApplicationArchive::require;

int
main(int argc, char* argv[])
{
    try {
        require(argc == 2, "Expected a test output directory.");
        const auto directory = std::filesystem::u8path(argv[1]);
        const auto self = std::filesystem::absolute(std::filesystem::u8path(argv[0]));
        const auto path = directory / "native-runtime.log";
        {
            std::ofstream seed(path, std::ios::binary);
            seed << "EXISTING\n";
        }
        for (int repetition = 0; repetition < 2; ++repetition) {
            Log log;
            log.open(path, self);
            log.startCapture();
            const std::string bytes(100000, '~');
            require(fwrite(bytes.data(), 1, bytes.size(), stdout) == bytes.size(),
                "Short output write.");
            fputs("\nSTDOUT_END\n", stdout);
            fputs("STDERR_END\n", stderr);
            require(log.finish(), "Log capture failed.");
            require(log.finish(), "Repeated finalization failed.");
        }
        std::ifstream input(path, std::ios::binary);
        const std::string text((std::istreambuf_iterator<char>(input)), {});
        require(text.rfind("EXISTING\n", 0) == 0, "Existing log was overwritten.");
        require(std::count(text.begin(), text.end(), '~') == 200000, "Lost output bytes.");
        require(text.find("STDERR_END") != std::string::npos, "Missing stderr.");
        input.close();
        // Retaining and writing to an inherited pipe must not delay finalization.
        Log inherited;
        inherited.open(directory / "descendant.log", self);
        inherited.startCapture();
#ifdef _WIN32
        Handle writer(duplicate(GetStdHandle(STD_OUTPUT_HANDLE)));
#else
        Handle writer(duplicate(STDOUT_FILENO));
#endif
        std::atomic<bool> stopped { false };
        std::thread descendant([&]() {
#ifndef _WIN32
            sigset_t signals;
            sigemptyset(&signals);
            sigaddset(&signals, SIGPIPE);
            pthread_sigmask(SIG_BLOCK, &signals, nullptr);
#endif
            while (!stopped) {
                if (!writeAll(writer.get(), "child\n", 6))
                    break;
                std::this_thread::sleep_for(std::chrono::milliseconds(1));
            }
        });
        const auto start = std::chrono::steady_clock::now();
        const bool completed = inherited.finish();
        const auto duration = std::chrono::steady_clock::now() - start;
        stopped = true;
        descendant.join();
        require(completed && duration < std::chrono::seconds(2),
            "Inherited writer blocked finalization.");
        Log disabled;
        disabled.startCapture();
        require(!disabled.enabled() && !disabled.capturing() && disabled.finish(),
            "Disabled log was active.");
        const auto encoded = Nelson::ApplicationStartup::encodeLogFilename("example.log");
        require(Nelson::ApplicationStartup::decodeLogFilename(encoded) == "example.log",
            "Invalid filename round trip.");
        std::cout << "RUNTIME_LOG_NATIVE_OK\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
