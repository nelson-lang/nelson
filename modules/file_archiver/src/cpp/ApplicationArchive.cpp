//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ApplicationArchive.hpp"
#include "ApplicationDirectory.hpp"
#include "ApplicationArchiveFormat.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "ApplicationArchiveIO.hpp"
#include <mz_compat.h>
#include <cstdio>
#include <memory>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    using ApplicationArchive::createFile;
    using ApplicationArchive::require;
    using ApplicationArchive::transfer;
    constexpr uint64_t maxExpandedBytes = 512ULL * 1024 * 1024;
    constexpr const char* temporaryArchiveName = ".nelson-payload.zip";
    fs::path
    nativePath(const std::wstring& value)
    {
        return fs::u8path(wstring_to_utf8(value));
    }
    //=============================================================================
    void
    readPayload(
        const fs::path& executable, const ApplicationArchive::Trailer& trailer, FILE* output)
    {
        const auto source = trailer.externalArchive
            ? ApplicationArchive::externalArchivePath(executable)
            : executable;
        require(fs::is_regular_file(source),
            "Application archive file is missing or not a regular file.");
        std::ifstream input(source, std::ios::binary | std::ios::ate);
        require(input.good(), "Cannot open application archive data.");
        if (trailer.externalArchive) {
            require(input.tellg() == static_cast<std::streamoff>(trailer.length),
                "External application archive size does not match the executable.");
        }
        input.seekg(trailer.externalArchive ? 0 : static_cast<std::streamoff>(trailer.offset));
        require(transfer(input, output, trailer.length) == trailer.digest,
            "Application archive integrity check failed.");
    }
    //=============================================================================
    void
    validateZip(const fs::path& archive, const fs::path& root)
    {
        unzFile file = unzOpen64(archive.u8string().c_str());
        require(file != nullptr, "Cannot open embedded ZIP archive.");
        struct Closer
        {
            unzFile file;
            ~Closer() { unzClose(file); }
        } closer { file };
        unz_global_info64 global {};
        require(unzGetGlobalInfo64(file, &global) == UNZ_OK && global.number_entry > 0
                && global.number_entry <= 10000,
            "Invalid embedded ZIP entry count.");
        uint64_t expanded = 0;
        std::set<std::wstring> paths;
        for (uint64_t i = 0; i < global.number_entry; ++i) {
            unz_file_info64 info {};
            std::array<char, 8192> name {};
            require(unzGetCurrentFileInfo64(file, &info, name.data(),
                        static_cast<uint16_t>(name.size()), nullptr, 0, nullptr, 0)
                    == UNZ_OK,
                "Cannot read embedded ZIP entry.");
            require(info.size_filename > 0 && info.size_filename < name.size()
                    && std::char_traits<char>::length(name.data()) == info.size_filename,
                "Invalid embedded ZIP filename.");
            require(info.uncompressed_size <= maxExpandedBytes - expanded,
                "Expanded application archive size limit exceeded.");
            expanded += info.uncompressed_size;
            require((info.flag & 1) == 0 && ((info.external_fa >> 16) & 0170000) != 0120000,
                "Encrypted entries and symbolic links are not supported in application archives.");
            std::wstring target;
            require(ApplicationArchive::validRelativePath(name.data())
                    && resolveSafeZipEntryPath(
                        root.generic_wstring(), utf8_to_wstring(name.data()), target),
                "Unsafe application archive path.");
            auto key = nativePath(target).lexically_normal().generic_wstring();
            while (!key.empty() && key.back() == L'/') {
                key.pop_back();
            }
            std::transform(key.begin(), key.end(), key.begin(),
                [](wchar_t c) { return c >= L'A' && c <= L'Z' ? c + (L'a' - L'A') : c; });
            require(paths.insert(key).second, "Duplicate application archive path.");
            require(nativePath(key).filename() != temporaryArchiveName,
                "Reserved application archive filename.");
            if (i + 1 < global.number_entry) {
                require(unzGoToNextFile(file) == UNZ_OK, "Cannot read embedded ZIP directory.");
            }
        }
    }
    //=============================================================================
    void
    extractZipFile(unzFile archive, const fs::path& target, uint64_t expected)
    {
        fs::create_directories(target.parent_path());
        FILE* output = createFile(target);
        bool opened = false;
        try {
            require(unzOpenCurrentFile(archive) == UNZ_OK, "Cannot open application ZIP entry.");
            opened = true;
            uint64_t written = 0;
            std::array<char, 32768> block;
            for (;;) {
                const int count = unzReadCurrentFile(
                    archive, block.data(), static_cast<unsigned>(block.size()));
                require(count >= 0, "Cannot decompress application ZIP entry.");
                if (count == 0) {
                    break;
                }
                require(static_cast<uint64_t>(count) <= expected - written,
                    "Application ZIP entry exceeds its declared size.");
                require(fwrite(block.data(), 1, count, output) == static_cast<size_t>(count),
                    "Cannot write application ZIP entry.");
                written += count;
            }
            require(written == expected, "Truncated application ZIP entry.");
            const auto checksum = unzCloseCurrentFile(archive);
            opened = false;
            require(checksum == UNZ_OK, "Application ZIP checksum mismatch.");
            const auto status = fclose(output);
            output = nullptr;
            require(status == 0, "Cannot close extracted application file.");
        } catch (...) {
            if (opened) {
                unzCloseCurrentFile(archive);
            }
            if (output != nullptr) {
                fclose(output);
            }
            throw;
        }
    }
    //=============================================================================
    void
    extractZip(const fs::path& archive, const fs::path& root)
    {
        unzFile file = unzOpen64(archive.u8string().c_str());
        require(file != nullptr, "Cannot open embedded ZIP archive.");
        struct Closer
        {
            unzFile file;
            ~Closer() { unzClose(file); }
        } closer { file };
        unz_global_info64 global {};
        require(unzGetGlobalInfo64(file, &global) == UNZ_OK, "Cannot read embedded ZIP directory.");
        for (uint64_t i = 0; i < global.number_entry; ++i) {
            unz_file_info64 info {};
            std::array<char, 8192> name {};
            require(unzGetCurrentFileInfo64(file, &info, name.data(),
                        static_cast<uint16_t>(name.size()), nullptr, 0, nullptr, 0)
                    == UNZ_OK,
                "Cannot read application ZIP entry.");
            const auto target = root / fs::u8path(name.data());
            if (name[info.size_filename - 1] == '/') {
                require(info.uncompressed_size == 0, "Invalid application ZIP directory.");
                fs::create_directories(target);
            } else {
                extractZipFile(file, target, info.uncompressed_size);
            }
            if (i + 1 < global.number_entry) {
                require(unzGoToNextFile(file) == UNZ_OK, "Cannot read application ZIP directory.");
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
createApplicationDirectory(const std::wstring& directory)
{
    ApplicationArchive::createPrivateDirectory(nativePath(directory));
}
//=============================================================================
void
verifyApplicationArchive(const std::wstring& executableName)
{
    const auto executable = nativePath(executableName);
    readPayload(executable, ApplicationArchive::read(executable), nullptr);
}
//=============================================================================
void
extractApplicationArchive(const std::wstring& executableName, const std::wstring& directoryName)
{
    const auto executable = nativePath(executableName);
    const auto root = fs::canonical(nativePath(directoryName));
    require(fs::is_directory(root) && fs::is_empty(root),
        "Application extraction requires an empty directory.");
    const auto trailer = ApplicationArchive::read(executable);
    const auto archive = root / temporaryArchiveName;
    FILE* output = createFile(archive);
    try {
        readPayload(executable, trailer, output);
        const auto status = fclose(output);
        output = nullptr;
        require(status == 0, "Cannot close temporary application archive.");
        validateZip(archive, root);
        extractZip(archive, root);
        fs::remove(archive);
    } catch (...) {
        if (output != nullptr) {
            fclose(output);
        }
        std::error_code ignored;
        fs::remove(archive, ignored);
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
