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
#include "ArchivePath.hpp"
#include "ApplicationDirectory.hpp"
#include "ApplicationArchiveFormat.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "ApplicationArchiveIO.hpp"
#include "ZipArchive.hpp"
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
    // Opens the extracted payload; every failure becomes the archive's own message.
    ZipArchive
    openEmbeddedZip(const fs::path& archive)
    {
        try {
            return ZipArchive::openFile(
                utf8_to_wstring(archive.u8string()), ZipArchive::Mode::Read);
        } catch (const ZipArchiveError&) {
            throw std::runtime_error("Cannot open embedded ZIP archive.");
        }
    }
    //=============================================================================
    void
    validateZip(const fs::path& archive, const fs::path& root)
    {
        ZipArchive zip = openEmbeddedZip(archive);
        const auto& entries = zip.entries();
        require(!entries.empty() && entries.size() <= 10000, "Invalid embedded ZIP entry count.");
        uint64_t expanded = 0;
        std::set<std::wstring> paths;
        for (const ZipEntryInfo& entry : entries) {
            require(
                !entry.name.empty() && entry.name.size() < 8192, "Invalid embedded ZIP filename.");
            require(entry.size <= maxExpandedBytes - expanded,
                "Expanded application archive size limit exceeded.");
            expanded += entry.size;
            require(!entry.isEncrypted && ((entry.externalAttributes >> 16) & 0170000) != 0120000,
                "Encrypted entries and symbolic links are not supported in application archives.");
            std::wstring target;
            require(ApplicationArchive::validRelativePath(entry.name)
                    && resolveSafeZipEntryPath(
                        root.generic_wstring(), utf8_to_wstring(entry.name), target),
                "Unsafe application archive path.");
            auto key = toNativePath(target).lexically_normal().generic_wstring();
            while (!key.empty() && key.back() == L'/') {
                key.pop_back();
            }
            std::transform(key.begin(), key.end(), key.begin(),
                [](wchar_t c) { return c >= L'A' && c <= L'Z' ? c + (L'a' - L'A') : c; });
            require(paths.insert(key).second, "Duplicate application archive path.");
            require(toNativePath(key).filename() != temporaryArchiveName,
                "Reserved application archive filename.");
        }
    }
    //=============================================================================
    void
    extractZipFile(ZipArchive& zip, size_t index, const fs::path& target)
    {
        fs::create_directories(target.parent_path());
        FILE* output = createFile(target);
        try {
            zip.readEntry(index, [output](const uint8_t* data, size_t size) {
                require(
                    fwrite(data, 1, size, output) == size, "Cannot write application ZIP entry.");
            });
        } catch (const ZipArchiveError& error) {
            fclose(output);
            switch (error.code()) {
            case ZipErrorCode::CrcMismatch:
                throw std::runtime_error("Application ZIP checksum mismatch.");
            case ZipErrorCode::EntryTooLarge:
                throw std::runtime_error("Application ZIP entry exceeds its declared size.");
            case ZipErrorCode::EntryTruncated:
                throw std::runtime_error("Truncated application ZIP entry.");
            default:
                throw std::runtime_error("Cannot decompress application ZIP entry.");
            }
        } catch (...) {
            fclose(output);
            throw;
        }
        require(fclose(output) == 0, "Cannot close extracted application file.");
    }
    //=============================================================================
    void
    extractZip(const fs::path& archive, const fs::path& root)
    {
        ZipArchive zip = openEmbeddedZip(archive);
        const auto& entries = zip.entries();
        for (size_t i = 0; i < entries.size(); ++i) {
            const ZipEntryInfo& entry = entries[i];
            const auto target = root / fs::u8path(entry.name);
            if (entry.name.back() == '/') {
                require(entry.size == 0, "Invalid application ZIP directory.");
                fs::create_directories(target);
            } else {
                extractZipFile(zip, i, target);
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
createApplicationDirectory(const std::wstring& directory)
{
    ApplicationArchive::createPrivateDirectory(toNativePath(directory));
}
//=============================================================================
void
verifyApplicationArchive(const std::wstring& executableName)
{
    const auto executable = toNativePath(executableName);
    readPayload(executable, ApplicationArchive::read(executable), nullptr);
}
//=============================================================================
void
extractApplicationArchive(const std::wstring& executableName, const std::wstring& directoryName)
{
    const auto executable = toNativePath(executableName);
    const auto root = fs::canonical(toNativePath(directoryName));
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
