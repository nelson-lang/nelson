//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationExecutable.hpp"
#include "ApplicationLauncherContract.hpp"
#include "ApplicationStartupConfiguration.hpp"
#include "ApplicationWindowsResources.hpp"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <algorithm>
#include <cstdio>
#include <filesystem>
#include <memory>
#include <stdexcept>
#include <vector>
#ifdef _WIN32
#include <windows.h>
#endif
namespace Nelson {
//=============================================================================
namespace {
    std::vector<uint8_t>
    launcherBytes(const std::string& filename, bool requireUnsealed = false)
    {
        using ApplicationArchive::require;
        require(!filename.empty() && filename.find('\0') == std::string::npos,
            "An application launcher filename is required.");
        std::ifstream input(std::filesystem::u8path(filename), std::ios::binary | std::ios::ate);
        require(input.good(), "Cannot open application launcher.");
        const auto position = input.tellg();
        require(position >= static_cast<std::streamoff>(ApplicationLauncher::recordSize),
            "Missing application launcher contract; rebuild the native launchers.");
        const auto size = static_cast<uint64_t>(position);
        auto prefix = size;
        if (size >= ApplicationArchive::trailerSize) {
            std::array<uint8_t, ApplicationArchive::trailerSize> bytes {};
            input.seekg(-static_cast<std::streamoff>(bytes.size()), std::ios::end);
            input.read(reinterpret_cast<char*>(bytes.data()), bytes.size());
            require(input.good(), "Cannot read application launcher trailer.");
            const std::string magic = "NLSAPP01";
            if (std::equal(magic.begin(), magic.end(), bytes.begin())) {
                require(!requireUnsealed, "Cannot configure an already packaged application.");
                prefix = ApplicationArchive::decode(bytes, size).offset;
            }
        }
        require(prefix <= 64ULL * 1024 * 1024, "Application launcher size limit exceeded.");
        std::vector<uint8_t> bytes(static_cast<size_t>(prefix));
        input.seekg(0);
        input.read(reinterpret_cast<char*>(bytes.data()), bytes.size());
        require(input.good(), "Cannot read application launcher contract.");
        return bytes;
    }
    //=============================================================================
    size_t
    recordOffset(
        const std::vector<uint8_t>& bytes, const std::array<uint8_t, 16>& marker, size_t size)
    {
        std::array<uint8_t, 20> signature {};
        std::copy(marker.begin(), marker.end(), signature.begin());
        ApplicationArchive::writeInteger(signature.data() + marker.size(), size, 4);
        const auto match
            = std::search(bytes.begin(), bytes.end(), signature.begin(), signature.end());
        ApplicationArchive::require(
            match != bytes.end() && static_cast<size_t>(bytes.end() - match) >= size,
            "Missing or truncated application launcher record; rebuild the native launchers.");
        ApplicationArchive::require(
            std::search(match + 1, bytes.end(), signature.begin(), signature.end()) == bytes.end(),
            "Ambiguous application launcher contract.");
        return static_cast<size_t>(match - bytes.begin());
    }
    //=============================================================================
    void
    copyBuildFile(const std::filesystem::path& source, const std::filesystem::path& target,
        std::vector<std::filesystem::path>& created)
    {
        FILE* input = nullptr;
        FILE* output = nullptr;
#ifdef _WIN32
        _wfopen_s(&input, source.c_str(), L"rb");
#else
        input = fopen(source.c_str(), "rb");
#endif
        if (!input) {
            throw std::runtime_error("Cannot read staged application output.");
        }
        const auto closeFile = [](FILE* file) { fclose(file); };
        std::unique_ptr<FILE, decltype(closeFile)> reader(input, closeFile);
#ifdef _WIN32
        _wfopen_s(&output, target.c_str(), L"wbx");
#else
        output = fopen(target.c_str(), "wbx");
#endif
        if (!output) {
            throw std::runtime_error(
                "Cannot publish application output; destination must not exist.");
        }
        std::unique_ptr<FILE, decltype(closeFile)> writer(output, closeFile);
        created.push_back(target);
        std::array<char, 65536> buffer;
        size_t count;
        while ((count = fread(buffer.data(), 1, buffer.size(), input)) != 0) {
            if (fwrite(buffer.data(), 1, count, output) != count) {
                throw std::runtime_error("Cannot write application output.");
            }
        }
        if (ferror(input) || fflush(output) != 0) {
            throw std::runtime_error("Cannot finish application output.");
        }
        if (fclose(writer.release()) != 0) {
            throw std::runtime_error("Cannot close application output.");
        }
        std::filesystem::permissions(target, std::filesystem::status(source).permissions());
    }
    //=============================================================================
} // namespace
//=============================================================================
std::string
applicationLauncherContract(const std::string& filename)
{
    const auto bytes = launcherBytes(filename);
    const auto offset
        = recordOffset(bytes, ApplicationLauncher::signature, ApplicationLauncher::recordSize);
    const auto value = ApplicationLauncher::decode(bytes.data() + offset);
    const char* kind = value.kind == ApplicationLauncher::Kind::Console ? "console"
        : value.kind == ApplicationLauncher::Kind::Windowed             ? "windowed"
                                                                        : "compiler";
    return nlohmann::json(
        { { "format", value.format }, { "startupInterface", value.startupInterface },
            { "archiveMinVersion", value.archiveMinVersion },
            { "archiveMaxVersion", value.archiveMaxVersion },
            { "runtimeDiscoveryVersion", value.runtimeDiscoveryVersion }, { "kind", kind } })
        .dump();
}
//=============================================================================
void
validateApplicationResourceTarget(const std::wstring& filename)
{
    const auto bytes = launcherBytes(wstring_to_utf8(filename), true);
    const auto offset
        = recordOffset(bytes, ApplicationLauncher::signature, ApplicationLauncher::recordSize);
    const auto contract = ApplicationLauncher::decode(bytes.data() + offset);
    ApplicationArchive::require(contract.kind != ApplicationLauncher::Kind::Compiler,
        "An application launcher is required.");
    if (contract.format >= 2) {
        const auto position
            = recordOffset(bytes, ApplicationStartup::signature, ApplicationStartup::recordSize);
        ApplicationArchive::require(ApplicationStartup::decode(bytes.data() + position).length == 0,
            "Application resources must be configured before startup options.");
    }
}
//=============================================================================
void
validateApplicationRuntimeLog(const std::string& filename)
{
    try {
        ApplicationStartup::validateLogFilename(filename);
    } catch (const std::exception& error) {
        throw std::invalid_argument(error.what());
    }
}
//=============================================================================
void
setApplicationRuntimeLog(const std::string& filename, const std::string& logFilename)
{
    const auto configuration = ApplicationStartup::encodeLogFilename(logFilename);
    auto bytes = launcherBytes(filename, true);
    const auto contractOffset
        = recordOffset(bytes, ApplicationLauncher::signature, ApplicationLauncher::recordSize);
    const auto contract = ApplicationLauncher::decode(bytes.data() + contractOffset);
    ApplicationArchive::require(
        contract.format >= 2 && contract.kind != ApplicationLauncher::Kind::Compiler,
        "This native launcher does not support runtime logging; rebuild the native launchers.");
    const auto offset
        = recordOffset(bytes, ApplicationStartup::signature, ApplicationStartup::recordSize);
    const auto location = ApplicationStartup::decode(bytes.data() + offset);
    ApplicationArchive::require(
        location.length == 0, "Application startup configuration already exists.");
#ifdef __APPLE__
    // macOS never rewrites the startup record in place (it would break the
    // Mach-O signature): the configuration is appended with a footer. Refuse a
    // second configuration when a footer is already present.
    ApplicationArchive::require(bytes.size() < ApplicationStartup::footerSize
            || !std::equal(ApplicationStartup::footerSignature.begin(),
                ApplicationStartup::footerSignature.end(),
                bytes.end() - ApplicationStartup::footerSize),
        "Application startup configuration already exists.");
    const auto footer = ApplicationStartup::footer(configuration.size());
    std::ofstream output(std::filesystem::u8path(filename), std::ios::binary | std::ios::app);
    ApplicationArchive::require(output.good(), "Cannot update application startup configuration.");
    output.write(reinterpret_cast<const char*>(configuration.data()), configuration.size());
    output.write(reinterpret_cast<const char*>(footer.data()), footer.size());
#else
    ApplicationArchive::writeInteger(bytes.data() + offset + 24, bytes.size(), 8);
    ApplicationArchive::writeInteger(bytes.data() + offset + 32, configuration.size(), 8);
    std::fstream output(
        std::filesystem::u8path(filename), std::ios::binary | std::ios::in | std::ios::out);
    ApplicationArchive::require(output.good(), "Cannot update application startup configuration.");
    output.seekp(0, std::ios::end);
    output.write(reinterpret_cast<const char*>(configuration.data()), configuration.size());
    output.seekp(static_cast<std::streamoff>(offset));
    output.write(
        reinterpret_cast<const char*>(bytes.data() + offset), ApplicationStartup::recordSize);
#endif
    output.flush();
    ApplicationArchive::require(output.good(), "Cannot write application startup configuration.");
}
//=============================================================================
void
publishApplicationFiles(const std::wstring& source, const std::wstring& destination)
{
    namespace fs = std::filesystem;
    if (source.empty() || destination.empty() || source.find(L'\0') != std::wstring::npos
        || destination.find(L'\0') != std::wstring::npos) {
        throw std::invalid_argument("Application output directories must be nonempty paths.");
    }
    const auto from = fs::u8path(wstring_to_utf8(source));
    const auto to = fs::u8path(wstring_to_utf8(destination));
    if (!fs::is_directory(from) || !fs::is_directory(to) || fs::equivalent(from, to)) {
        throw std::invalid_argument(
            "Distinct existing application output directories are required.");
    }
    std::vector<fs::path> files;
    for (const auto& item : fs::directory_iterator(from)) {
        if (!fs::is_regular_file(item.symlink_status()) || files.size() >= 64
            || fs::symlink_status(to / item.path().filename()).type() != fs::file_type::not_found) {
            throw std::runtime_error(
                "Application outputs must be regular files with new destinations.");
        }
        files.push_back(item.path());
    }
    if (files.empty()) {
        throw std::invalid_argument("No application output files to publish.");
    }
    std::sort(files.begin(), files.end(), [](const auto& a, const auto& b) {
        if (a.filename() == "buildresult.json") {
            return false;
        }
        if (b.filename() == "buildresult.json") {
            return true;
        }
        return a < b;
    });
    std::vector<fs::path> created;
    created.reserve(files.size());
    try {
        for (const auto& file : files) {
            copyBuildFile(file, to / file.filename(), created);
        }
    } catch (...) {
        for (const auto& file : created) {
            std::error_code ignored;
            fs::remove(file, ignored);
        }
        throw;
    }
}
//=============================================================================
std::array<uint16_t, 4>
parseApplicationVersion(const std::wstring& version)
{
    std::array<uint16_t, 4> parts {};
    size_t part = 0;
    bool digit = false;
    if (version.empty() || version.size() > 23) {
        throw std::invalid_argument(
            "ExecutableVersion requires one to four decimal components (0 to 65535).");
    }
    for (const auto character : version) {
        if (character == L'.' && digit && part < 3) {
            ++part;
            digit = false;
        } else if (character >= L'0' && character <= L'9') {
            const auto value = static_cast<uint32_t>(parts[part]) * 10 + character - L'0';
            if (value > 65535) {
                throw std::invalid_argument("ExecutableVersion components must not exceed 65535.");
            }
            parts[part] = static_cast<uint16_t>(value);
            digit = true;
        } else {
            throw std::invalid_argument(
                "ExecutableVersion requires one to four decimal components (0 to 65535).");
        }
    }
    if (!digit) {
        throw std::invalid_argument("ExecutableVersion must not end with a period.");
    }
    return parts;
}
#ifdef _WIN32
namespace {
    using Bytes = std::vector<uint8_t>;
    void
    appendWord(Bytes& bytes, uint16_t value)
    {
        bytes.push_back(static_cast<uint8_t>(value));
        bytes.push_back(static_cast<uint8_t>(value >> 8));
    }
    //=============================================================================
    void
    appendText(Bytes& bytes, const std::wstring& text)
    {
        for (const auto character : text) {
            appendWord(bytes, static_cast<uint16_t>(character));
        }
        appendWord(bytes, 0);
    }
    //=============================================================================
    void
    alignResource(Bytes& bytes)
    {
        while (bytes.size() % 4) {
            bytes.push_back(0);
        }
    }
    //=============================================================================
    Bytes
    versionBlock(const std::wstring& key, uint16_t type, uint16_t valueLength, const Bytes& value,
        const std::vector<Bytes>& children = {})
    {
        Bytes bytes;
        appendWord(bytes, 0);
        appendWord(bytes, valueLength);
        appendWord(bytes, type);
        appendText(bytes, key);
        alignResource(bytes);
        bytes.insert(bytes.end(), value.begin(), value.end());
        for (const auto& child : children) {
            alignResource(bytes);
            bytes.insert(bytes.end(), child.begin(), child.end());
        }
        bytes[0] = static_cast<uint8_t>(bytes.size());
        bytes[1] = static_cast<uint8_t>(bytes.size() >> 8);
        return bytes;
    }
    //=============================================================================
    Bytes
    versionResource(const std::array<uint16_t, 4>& parts)
    {
        VS_FIXEDFILEINFO fixed {};
        fixed.dwSignature = VS_FFI_SIGNATURE;
        fixed.dwStrucVersion = VS_FFI_STRUCVERSION;
        fixed.dwFileVersionMS = MAKELONG(parts[1], parts[0]);
        fixed.dwFileVersionLS = MAKELONG(parts[3], parts[2]);
        fixed.dwProductVersionMS = fixed.dwFileVersionMS;
        fixed.dwProductVersionLS = fixed.dwFileVersionLS;
        fixed.dwFileFlagsMask = VS_FFI_FILEFLAGSMASK;
        fixed.dwFileOS = VOS_NT_WINDOWS32;
        fixed.dwFileType = VFT_APP;
        const auto* begin = reinterpret_cast<const uint8_t*>(&fixed);
        Bytes fixedValue(begin, begin + sizeof(fixed));
        const auto text = std::to_wstring(parts[0]) + L"." + std::to_wstring(parts[1]) + L"."
            + std::to_wstring(parts[2]) + L"." + std::to_wstring(parts[3]);
        Bytes stringValue;
        appendText(stringValue, text);
        const auto length = static_cast<uint16_t>(text.size() + 1);
        const auto strings = versionBlock(L"StringFileInfo", 1, 0, {},
            { versionBlock(L"040904b0", 1, 0, {},
                { versionBlock(L"FileVersion", 1, length, stringValue),
                    versionBlock(L"ProductVersion", 1, length, stringValue) }) });
        // Version resource blocks are DWORD-aligned; translation uses US English, UTF-16.
        Bytes translation;
        appendWord(translation, 0x0409);
        appendWord(translation, 1200);
        const auto variables = versionBlock(
            L"VarFileInfo", 1, 0, {}, { versionBlock(L"Translation", 0, 4, translation) });
        return versionBlock(
            L"VS_VERSION_INFO", 0, sizeof(fixed), fixedValue, { strings, variables });
    }
    //=============================================================================
} // namespace
#endif
void
setApplicationVersion(const std::wstring& filename, const std::wstring& version)
{
    const auto parts = parseApplicationVersion(version);
    if (filename.empty() || filename.find(L'\0') != std::wstring::npos) {
        throw std::invalid_argument("An application launcher filename is required.");
    }
#ifdef _WIN32
    validateApplicationResourceTarget(filename);
    auto resource = versionResource(parts);
    ApplicationWindowsResources update(filename);
    update.set(MAKEINTRESOURCEW(16), MAKEINTRESOURCEW(1), MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL),
        resource);
    update.commit();
#else
    (void)parts;
    (void)filename;
    throw std::runtime_error("Executable version resources require Windows.");
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
