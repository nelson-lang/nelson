//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "RuntimeLogConsole.hpp"
#include <atomic>
#include <chrono>
#include <cstring>
#include <memory>
#include <mutex>
#include <thread>
#ifndef _WIN32
#include <pthread.h>
#include <signal.h>
#endif
namespace Nelson::RuntimeLogging {
class Log
{
    class Stream;

public:
    Log() = default;
    ~Log() { finish(); }
    Log(const Log&) = delete;
    Log&
    operator=(const Log&)
        = delete;

    void
    open(const std::filesystem::path& filename, const std::filesystem::path& executable)
    {
        const auto path = filename.is_absolute() ? filename : executable.parent_path() / filename;
        file.reset(openLog(path, executable));
        const std::string message = "nelsonc: Initializing application runtime.\n";
        ApplicationArchive::require(writeAll(file.get(), message.data(), message.size()),
            "Cannot write application runtime log.");
    }
    bool
    enabled() const
    {
        return valid(file.get());
    }
    bool
    capturing() const
    {
        return error != nullptr && error->active();
    }
    void
    startCapture();
    bool
    finish() noexcept;
    NativeHandle
    originalOutput(bool error) const;
    void
    diagnostic(const std::string& text)
    {
        if (enabled() && !capturing()) {
            append(text.data(), text.size());
        }
    }

private:
    void
    append(const char* bytes, size_t length)
    {
        std::lock_guard<std::mutex> guard(mutex);
        if (!writeAll(file.get(), bytes, length)) {
            failed = true;
        }
    }
    class Stream
    {
    public:
        Stream(Log& log, FILE* stream, bool error) : log(log), stream(stream), error(error)
        {
#ifdef _WIN32
            saved.reset(duplicate(GetStdHandle(error ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE)));
            const auto fd = _fileno(stream);
            if (fd >= 0) {
                savedCrt = _dup(fd);
                ApplicationArchive::require(
                    savedCrt >= 0, "Cannot preserve application output stream.");
            }
#else
            saved.reset(duplicate(fileno(stream)));
#endif
            try {
                makePipe(reader, writer);
            } catch (...) {
#ifdef _WIN32
                if (savedCrt >= 0) {
                    _close(savedCrt);
                }
#endif
                throw;
            }
        }
        ~Stream()
        {
            restore();
            stop = true;
            if (worker.joinable()) {
                worker.join();
            }
#ifdef _WIN32
            if (savedCrt >= 0) {
                _close(savedCrt);
            }
#endif
        }
        void
        start()
        {
            worker = std::thread([this]() { consume(); });
            fflush(stream);
#ifdef _WIN32
            if (_fileno(stream) < 0) {
                FILE* result = nullptr;
                ApplicationArchive::require(freopen_s(&result, "NUL", "wb", stream) == 0,
                    "Cannot initialize application output stream.");
            }
            Handle copy(duplicate(writer.get()));
            const int descriptor
                = _open_osfhandle(reinterpret_cast<intptr_t>(copy.get()), _O_BINARY | _O_WRONLY);
            ApplicationArchive::require(descriptor >= 0, "Cannot attach runtime log stream.");
            copy.release();
            const auto result = _dup2(descriptor, _fileno(stream));
            _close(descriptor);
            ApplicationArchive::require(result == 0, "Cannot redirect runtime log stream.");
            redirected = true;
            ApplicationArchive::require(
                SetStdHandle(error ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE, writer.get()) != 0,
                "Cannot redirect application output handle.");
#else
            ApplicationArchive::require(
                dup2(writer.get(), fileno(stream)) >= 0, "Cannot redirect runtime log stream.");
            redirected = true;
#endif
        }
        void
        restore() noexcept
        {
            if (!redirected) {
                return;
            }
            fflush(stream);
#ifdef _WIN32
            if (savedCrt >= 0) {
                _dup2(savedCrt, _fileno(stream));
            } else {
                _close(_fileno(stream));
            }
            SetStdHandle(error ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE,
                savedCrt >= 0 ? reinterpret_cast<HANDLE>(_get_osfhandle(_fileno(stream)))
                              : nullptr);
#else
            if (valid(saved.get())) {
                dup2(saved.get(), fileno(stream));
            } else {
                ::close(fileno(stream));
            }
#endif
            redirected = false;
        }
        NativeHandle
        original() const
        {
            return saved.get();
        }
        bool
        active() const
        {
            return redirected;
        }

    private:
        void
        consume() noexcept
        {
            try {
#ifndef _WIN32
                // A disconnected output consumer must not terminate the application.
                sigset_t signals;
                sigemptyset(&signals);
                sigaddset(&signals, SIGPIPE);
                if (pthread_sigmask(SIG_BLOCK, &signals, nullptr) != 0) {
                    log.failed = true;
                    return;
                }
#endif
                std::array<char, 32768> bytes {};
                size_t retained = 0;
                bool forward = valid(saved.get());
                OutputForwarder destination(saved.get());
                int remaining = -1;
                for (;;) {
                    // Drain the final snapshot, not an indefinitely writing descendant.
                    if (stop && remaining < 0) {
                        remaining = availableBytes(reader.get());
                    }
                    if (remaining == 0) {
                        break;
                    }
                    const auto room = bytes.size() - retained;
                    const auto capacity
                        = remaining < 0 ? room : (std::min)(room, static_cast<size_t>(remaining));
                    const auto count
                        = readAvailable(reader.get(), bytes.data() + retained, capacity);
                    if (count < 0) {
                        break;
                    }
                    if (count > 0) {
                        if (remaining > 0) {
                            remaining -= count;
                        }
                        // Keep incomplete codepoints out of interleaved log writes.
                        const auto total = retained + static_cast<size_t>(count);
                        const auto complete = completeUtf8Prefix(bytes.data(), total);
                        if (complete) {
                            log.append(bytes.data(), complete);
                        }
                        if (forward) {
                            forward = destination.write(
                                bytes.data() + retained, static_cast<size_t>(count));
                        }
                        retained = total - complete;
                        std::memmove(bytes.data(), bytes.data() + complete, retained);
                    } else {
                        if (stop) {
                            break;
                        }
                        std::this_thread::sleep_for(std::chrono::milliseconds(10));
                    }
                }
                // Preserve truncated or non-UTF-8 output verbatim at end of stream.
                if (retained) {
                    log.append(bytes.data(), retained);
                }
                if (forward) {
                    destination.finish();
                }
            } catch (...) {
                log.failed = true;
            }
        }
        Log& log;
        FILE* stream;
        bool error;
        bool redirected = false;
        Handle saved, reader, writer;
        std::thread worker;
        std::atomic<bool> stop { false };
#ifdef _WIN32
        int savedCrt = -1;
#endif
    };
    Handle file;
    std::mutex mutex;
    std::atomic<bool> failed { false };
    std::unique_ptr<Stream> output, error;
};
//=============================================================================
inline void
Log::startCapture()
{
    if (!enabled()) {
        return;
    }
    output = std::make_unique<Stream>(*this, stdout, false);
    error = std::make_unique<Stream>(*this, stderr, true);
    output->start();
    error->start();
}
//=============================================================================
inline bool
Log::finish() noexcept
{
    if (output) {
        output->restore();
    }
    if (error) {
        error->restore();
    }
    output.reset();
    error.reset();
    file.reset();
    return !failed;
}
//=============================================================================
inline NativeHandle
Log::originalOutput(bool isError) const
{
    return (isError ? error : output)->original();
}
//=============================================================================
} // namespace Nelson::RuntimeLogging
//=============================================================================
