//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include "ArchiveDiskWriter.hpp"
#include "LibarchiveSupport.hpp"
#include "ArchivePath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
[[noreturn]] static void
diskFailure(struct archive* disk, const std::wstring& path);
//=============================================================================
ArchiveDiskWriter::ArchiveDiskWriter()
{
    disk = archive_write_disk_new();
    if (disk == nullptr) {
        throw ZipArchiveError(ZipErrorCode::WriteFailed, "Cannot allocate disk writer.");
    }
    int flags = ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_SECURE_NODOTDOT
        | ARCHIVE_EXTRACT_SECURE_SYMLINKS;
    archive_write_disk_set_options(disk, flags);
}
//=============================================================================
ArchiveDiskWriter::~ArchiveDiskWriter()
{
    if (!currentFile.empty()) {
        abandonFile();
    }
    archive_write_free(disk);
}
//=============================================================================
void
ArchiveDiskWriter::folder(const std::wstring& path, time_t modified, uint32_t mode)
{
    writeHeader(path, AE_IFDIR, 0, modified, mode == 0 ? 0755 : mode, "");
    if (archive_write_finish_entry(disk) != ARCHIVE_OK) {
        diskFailure(disk, path);
    }
}
//=============================================================================
void
ArchiveDiskWriter::beginFile(const std::wstring& path, uint64_t size, time_t modified, uint32_t mode)
{
    writeHeader(path, AE_IFREG, size, modified, mode == 0 ? 0644 : mode, "");
    currentFile = path;
}
//=============================================================================
void
ArchiveDiskWriter::write(const uint8_t* data, size_t size)
{
    if (size > 0 && archive_write_data(disk, data, size) != static_cast<la_ssize_t>(size)) {
        diskFailure(disk, currentFile);
    }
}
//=============================================================================
void
ArchiveDiskWriter::endFile()
{
    const std::wstring path = currentFile;
    currentFile.clear();
    if (archive_write_finish_entry(disk) != ARCHIVE_OK) {
        diskFailure(disk, path);
    }
}
//=============================================================================
void
ArchiveDiskWriter::abandonFile()
{
    const std::wstring path = currentFile;
    currentFile.clear();
    archive_write_finish_entry(disk);
    std::error_code ec;
    std::filesystem::remove(toNativePath(path), ec);
}
//=============================================================================
void
ArchiveDiskWriter::link(const std::wstring& path, const std::string& target, time_t modified)
{
    writeHeader(path, AE_IFLNK, 0, modified, 0777, target);
    if (archive_write_finish_entry(disk) != ARCHIVE_OK) {
        diskFailure(disk, path);
    }
}
//=============================================================================
void
ArchiveDiskWriter::finish()
{
    if (archive_write_close(disk) != ARCHIVE_OK) {
        throw ZipArchiveError(
            ZipErrorCode::WriteFailed, libarchiveErrorText(disk, "Cannot set folder dates."));
    }
}
//=============================================================================
void
ArchiveDiskWriter::writeHeader(const std::wstring& path, unsigned int type, uint64_t size,
    time_t modified, uint32_t mode, const std::string& target)
{
    struct archive_entry* item = archive_entry_new();
#ifdef _WIN32
    archive_entry_copy_pathname_w(item, path.c_str());
#else
    archive_entry_set_pathname_utf8(item, wideToUtf8(path).c_str());
#endif
    archive_entry_set_filetype(item, type);
    archive_entry_set_perm(item, mode & 07777);
    archive_entry_set_size(item, static_cast<la_int64_t>(size));
    if (modified != 0) {
        archive_entry_set_mtime(item, modified, 0);
    }
    if (type == AE_IFLNK) {
        archive_entry_set_symlink_utf8(item, target.c_str());
    }
    const int status = archive_write_header(disk, item);
    archive_entry_free(item);
    if (status != ARCHIVE_OK && status != ARCHIVE_WARN) {
        diskFailure(disk, path);
    }
}
//=============================================================================
static void
diskFailure(struct archive* disk, const std::wstring& path)
{
    throw ZipArchiveError(ZipErrorCode::WriteFailed,
        libarchiveErrorText(disk, "Cannot write extracted file."), wideToUtf8(path));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
