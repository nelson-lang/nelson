//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <bcrypt.h>
#include <sddl.h>
#include <shellapi.h>
#include "InstallerIO.hpp"
#include "InstallerOptions.hpp"
#include "InstallerPayload.hpp"
#include <memory>
//=============================================================================
namespace Nelson::Installer {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    using ApplicationArchive::require;
    class Handle
    {
    public:
        explicit Handle(HANDLE value) : value(value) { }
        ~Handle()
        {
            if (value && value != INVALID_HANDLE_VALUE) {
                CloseHandle(value);
            }
        }
        Handle(const Handle&) = delete;
        Handle&
        operator=(const Handle&)
            = delete;
        HANDLE value;
    };
    //=============================================================================
    std::wstring
    nonce()
    {
        std::array<uint8_t, 16> bytes {};
        require(BCryptGenRandom(nullptr, bytes.data(), static_cast<ULONG>(bytes.size()),
                    BCRYPT_USE_SYSTEM_PREFERRED_RNG)
                == 0,
            "Cannot create an installation identifier.");
        std::wstring result;
        for (const auto byte : bytes) {
            result += L"0123456789abcdef"[byte >> 4];
            result += L"0123456789abcdef"[byte & 15];
        }
        return result;
    }
    //=============================================================================
    fs::path
    temporaryDirectory()
    {
        std::wstring buffer(32768, L'\0');
        const auto size = GetTempPathW(static_cast<DWORD>(buffer.size()), buffer.data());
        require(size > 0 && size < buffer.size(), "Cannot locate the temporary directory.");
        buffer.resize(size);
        return buffer;
    }
    //=============================================================================
    class Scratch
    {
    public:
        Scratch()
            : directory(temporaryDirectory() / (L"nelson-install-" + nonce()))
            , executable(directory / L"setup.exe")
        {
            PSECURITY_DESCRIPTOR descriptor = nullptr;
            // Elevation can run the installer under a different administrator account.
            require(ConvertStringSecurityDescriptorToSecurityDescriptorW(
                        L"D:P(A;;FA;;;OW)(A;;FA;;;SY)(A;;FA;;;BA)", SDDL_REVISION_1, &descriptor,
                        nullptr)
                    != FALSE,
                "Cannot secure the installation directory.");
            SECURITY_ATTRIBUTES attributes { sizeof(SECURITY_ATTRIBUTES), descriptor, FALSE };
            const auto created = CreateDirectoryW(directory.c_str(), &attributes);
            LocalFree(descriptor);
            require(created != FALSE, "Cannot create a private installation directory.");
        }
        ~Scratch()
        {
            DeleteFileW(executable.c_str());
            RemoveDirectoryW(directory.c_str());
        }
        Scratch(const Scratch&) = delete;
        Scratch&
        operator=(const Scratch&)
            = delete;
        fs::path directory;
        fs::path executable;
    };
    //=============================================================================
    void
    extractPayload(const fs::path& destination, const Options& options, bool documented)
    {
        std::wstring filename(32768, L'\0');
        const auto size
            = GetModuleFileNameW(nullptr, filename.data(), static_cast<DWORD>(filename.size()));
        require(size > 0 && size < filename.size(), "Cannot locate this installer.");
        filename.resize(size);
        std::ifstream input(fs::path(filename), std::ios::binary);
        require(input.good(), "Cannot read this installer.");
        const auto payload = readPayload(input, fs::file_size(filename));
        if (documented) {
            validateOptions(options, payload.runtime);
        }
        FILE* raw = nullptr;
        _wfopen_s(&raw, destination.c_str(), L"wbx");
        require(raw != nullptr, "Cannot create extracted installer.");
        std::unique_ptr<FILE, decltype(&fclose)> output(raw, fclose);
        const auto digest = transfer(input, output.get(), payload.length);
        require(digest == payload.digest, "Installer payload checksum does not match.");
        require(fclose(output.release()) == 0, "Cannot close the extracted installer.");
    }
    //=============================================================================
    int
    launch(const fs::path& executable, const std::wstring& arguments)
    {
        Handle image(CreateFileW(executable.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr,
            OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr));
        require(image.value != INVALID_HANDLE_VALUE, "Cannot lock the extracted installer.");
        std::wstring command = L"\"" + executable.native() + L"\" " + arguments;
        require(command.size() < 32767, "Installation command line is too long.");
        STARTUPINFOW startup {};
        startup.cb = sizeof(startup);
        PROCESS_INFORMATION process {};
        if (!CreateProcessW(executable.c_str(), command.data(), nullptr, nullptr, TRUE, 0, nullptr,
                nullptr, &startup, &process)) {
            throw std::runtime_error("Cannot start the installer (Windows error "
                + std::to_string(GetLastError()) + ").");
        }
        Handle thread(process.hThread);
        Handle child(process.hProcess);
        require(WaitForSingleObject(child.value, INFINITE) == WAIT_OBJECT_0,
            "Cannot wait for the installer.");
        DWORD code = 1;
        require(GetExitCodeProcess(child.value, &code) != FALSE,
            "Cannot read the installer exit code.");
        return static_cast<int>(code);
    }
    //=============================================================================
    void
    reportFailure(const Options& options, const char* message)
    {
        const auto text = std::string("Nelson installer: ") + message + "\r\n";
        DWORD written = 0;
        WriteFile(GetStdHandle(STD_ERROR_HANDLE), text.data(), static_cast<DWORD>(text.size()),
            &written, nullptr);
        if (!options.log.empty()) {
            Handle log(CreateFileW(options.log.c_str(), GENERIC_WRITE, FILE_SHARE_READ, nullptr,
                CREATE_NEW, FILE_ATTRIBUTE_NORMAL, nullptr));
            if (log.value != INVALID_HANDLE_VALUE) {
                WriteFile(
                    log.value, text.data(), static_cast<DWORD>(text.size()), &written, nullptr);
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
int
run(const wchar_t* rawArguments)
{
    Options options;
    try {
        options.log = temporaryDirectory() / (L"nelson_installer_" + nonce() + L".log");
        int count = 0;
        auto arguments = CommandLineToArgvW(GetCommandLineW(), &count);
        require(arguments != nullptr, "Cannot parse installation arguments.");
        std::vector<std::wstring> values(arguments + 1, arguments + count);
        LocalFree(arguments);
        const auto documented = std::any_of(values.begin(), values.end(),
            [](const auto& value) { return !value.empty() && value.front() == L'-'; });
        std::wstring parameters(rawArguments);
        if (documented) {
            parseOptions(values, options);
            parameters = nativeArguments(options);
        }
        Scratch scratch;
        extractPayload(scratch.executable, options, documented);
        return launch(scratch.executable, parameters);
    } catch (const std::exception& error) {
        reportFailure(options, error.what());
        return 1;
    }
}
//=============================================================================
} // namespace Nelson::Installer
//=============================================================================
int WINAPI
wWinMain(HINSTANCE, HINSTANCE, PWSTR arguments, int)
{
    return Nelson::Installer::run(arguments);
}
//=============================================================================
