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
#include "ApplicationArchiveFormat.hpp"
#include "ApplicationStartupConfiguration.hpp"
#include <cstdio>
#include <memory>
#ifdef __linux__
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
namespace Nelson::LauncherInputFile {
// Stateless deleter: using decltype(&std::fclose) as the deleter triggers
// -Wignored-attributes because glibc annotates fclose, and it also bloats the
// pointer with a stored function pointer.
struct FileCloser
{
    void
    operator()(std::FILE* file) const
    {
        if (file != nullptr) {
            std::fclose(file);
        }
    }
};
//=============================================================================
using File = std::unique_ptr<std::FILE, FileCloser>;
inline File
open(const std::filesystem::path& path)
{
#ifdef _WIN32
    std::FILE* file = nullptr;
    _wfopen_s(&file, path.c_str(), L"rb");
#elif defined(__linux__)
    const auto descriptor = ::open(path.c_str(), O_RDONLY | O_CLOEXEC | O_NONBLOCK);
    if (descriptor < 0) {
        return File(nullptr);
    }
    struct stat info {};
    if (::fstat(descriptor, &info) != 0 || !S_ISREG(info.st_mode)) {
        ::close(descriptor);
        return File(nullptr);
    }
    auto* file = ::fdopen(descriptor, "rb");
    if (!file) {
        ::close(descriptor);
    }
#else
    std::FILE* file = std::fopen(path.c_str(), "rb");
#endif
    return File(file);
}
//=============================================================================
inline ApplicationArchive::Trailer
readTrailer(const std::filesystem::path& path)
{
    using ApplicationArchive::require;
    auto file = open(path);
    require(file != nullptr, "Cannot open application executable.");
#ifdef _WIN32
    require(_fseeki64(file.get(), 0, SEEK_END) == 0, "Cannot read application trailer.");
    const auto size = _ftelli64(file.get());
    require(size >= static_cast<__int64>(ApplicationArchive::trailerSize),
        "Missing application archive.");
    require(_fseeki64(file.get(), -static_cast<__int64>(ApplicationArchive::trailerSize), SEEK_END)
            == 0,
        "Cannot read application trailer.");
#else
    require(fseeko(file.get(), 0, SEEK_END) == 0, "Cannot read application trailer.");
    const auto size = ftello(file.get());
    require(size >= static_cast<off_t>(ApplicationArchive::trailerSize),
        "Missing application archive.");
    require(fseeko(file.get(), -static_cast<off_t>(ApplicationArchive::trailerSize), SEEK_END) == 0,
        "Cannot read application trailer.");
#endif
    std::array<uint8_t, ApplicationArchive::trailerSize> bytes {};
    require(std::fread(bytes.data(), 1, bytes.size(), file.get()) == bytes.size(),
        "Cannot read application trailer.");
    return ApplicationArchive::decode(bytes, static_cast<uint64_t>(size));
}
//=============================================================================
inline std::string
readLogFilename(const std::filesystem::path& path, const ApplicationStartup::Location& location,
    uint64_t archiveOffset)
{
    using ApplicationArchive::require;
    auto file = open(path);
    require(file != nullptr, "Cannot open application startup configuration.");
    const auto size = std::filesystem::file_size(path);
    uint64_t offset = location.offset;
    uint64_t length = location.length;
    if (length == 0) {
        if (archiveOffset < ApplicationStartup::footerSize || archiveOffset > size) {
            return {};
        }
        const auto footerOffset = archiveOffset - ApplicationStartup::footerSize;
#ifdef _WIN32
        auto status = _fseeki64(file.get(), static_cast<__int64>(footerOffset), SEEK_SET);
#else
        auto status = fseeko(file.get(), static_cast<off_t>(footerOffset), SEEK_SET);
#endif
        require(status == 0, "Cannot seek application runtime log footer.");
        std::array<uint8_t, ApplicationStartup::footerSize> footer {};
        require(std::fread(footer.data(), 1, footer.size(), file.get()) == footer.size(),
            "Cannot read application runtime log footer.");
        if (!std::equal(ApplicationStartup::footerSignature.begin(),
                ApplicationStartup::footerSignature.end(), footer.begin())) {
            return {};
        }
        length = ApplicationStartup::decodeFooter(footer);
        require(length <= footerOffset, "Truncated application startup configuration.");
        offset = footerOffset - length;
    }
    require(
        offset <= size && length <= size - offset, "Truncated application startup configuration.");
#ifdef _WIN32
    const auto status = _fseeki64(file.get(), static_cast<__int64>(offset), SEEK_SET);
#else
    const auto status = fseeko(file.get(), static_cast<off_t>(offset), SEEK_SET);
#endif
    require(status == 0, "Cannot seek application startup configuration.");
    std::vector<uint8_t> bytes(static_cast<size_t>(length));
    require(std::fread(bytes.data(), 1, bytes.size(), file.get()) == bytes.size(),
        "Cannot read application startup configuration.");
    return ApplicationStartup::decodeLogFilename(bytes);
}
//=============================================================================
} // namespace Nelson::LauncherInputFile
//=============================================================================
