//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "movefileBuiltin.hpp"
#include "CopyFile.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonException.hpp"
#include "RemoveDirectory.hpp"
#include "RemoveFile.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
static bool
moveDirectory(
    const std::wstring& src, const std::wstring& dest, bool force, std::wstring& errorMessage);
//=============================================================================
static bool
moveFile(const std::wstring& src, const std::wstring& dest, bool force, std::wstring& errorMessage);
//=============================================================================
static bool
verifyTreeCopied(const nfs::path& src, const nfs::path& finalDest, std::wstring& errorMessage);
//=============================================================================
bool
moveOne(const std::wstring& src, const std::wstring& dest, bool force, std::wstring& errorMessage)
{
    bool permissionDenied = false;
    if (FileSystemWrapper::Path::is_regular_file(src, permissionDenied)) {
        return moveFile(src, dest, force, errorMessage);
    }
    if (permissionDenied) {
        Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
    }
    if (FileSystemWrapper::Path::is_directory(src, permissionDenied)) {
        return moveDirectory(src, dest, force, errorMessage);
    }
    if (permissionDenied) {
        Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
    }
    Error(L"Nelson:files_folders_functions:fileSourceDoesNotExist", L"File source does not exist.");
    return false;
}
//=============================================================================
bool
moveMany(const wstringVector& src, const std::wstring& dest, bool force, std::wstring& errorMessage)
{
    for (const auto& item : src) {
        if (!moveOne(item, dest, force, errorMessage)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::movefileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    bool force = false;
    if (argIn.size() == 3) {
        std::wstring arg3 = argIn[2].asWideString();
        if (arg3 == L"f" || arg3 == L"F") {
            force = true;
        } else {
            Error("Nelson:files_folders_functions:invalidOption", "'f' expected.");
        }
    }
    ArrayOf srcArg = argIn[0];
    if (!(srcArg.isRowVectorCharacterArray() || srcArg.isScalarStringArray() || srcArg.isCell()
            || srcArg.isStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    std::wstring dest = argIn[1].asWideString();
    std::wstring errorMessage;
    bool ok = false;
    try {
        if (srcArg.isRowVectorCharacterArray() || srcArg.isScalarStringArray()) {
            ok = moveOne(srcArg.asWideString(), dest, force, errorMessage);
        } else {
            ok = moveMany(srcArg.asWideStringVector(true), dest, force, errorMessage);
        }
    } catch (const NelsonException& e) {
        if (nLhs <= 1) {
            throw;
        }
        ok = false;
        errorMessage = e.message();
    }
    ArrayOfVector retval;
    if (nLhs == 0) {
        if (!ok) {
            Error(L"Nelson:movefile:moveFailed", errorMessage);
        }
    } else {
        retval << ArrayOf::logicalScalar(ok);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
namespace {
//=============================================================================
static bool
removeSourceDirectory(const std::wstring& src, std::wstring& errorMessage)
{
    return RemoveDirectory(src, true, errorMessage);
}
//=============================================================================
// The source is removed only once the destination file exists.
static bool
moveFile(const std::wstring& src, const std::wstring& dest, bool force, std::wstring& errorMessage)
{
    if (!CopyFile(src, dest, force, errorMessage)) {
        return false;
    }
    FileSystemWrapper::Path srcPath(src);
    FileSystemWrapper::Path destPath(dest);
    bool permissionDenied = false;
    if (FileSystemWrapper::Path::is_directory(dest, permissionDenied)) {
        destPath = destPath / srcPath.filename();
    }
    if (!FileSystemWrapper::Path::is_regular_file(destPath)) {
        // A forced copy can fail silently: keep the source.
        errorMessage = L"Cannot move file: destination was not created.";
        return false;
    }
    return RemoveFile(src, errorMessage);
}
//=============================================================================
// Destination missing: the source is renamed to it. Destination is a directory:
// the source goes inside it (destination/<source name>). The source is removed
// only after the whole tree is verified at the target.
static bool
moveDirectory(
    const std::wstring& src, const std::wstring& dest, bool force, std::wstring& errorMessage)
{
    nfs::path srcPath(src);
    nfs::path destPath(dest);

    std::error_code existsCode;
    bool destExists = nfs::exists(destPath, existsCode);
    bool destIsDir = destExists && nfs::is_directory(destPath, existsCode);

    nfs::path finalDest;
    if (destIsDir) {
        finalDest = destPath / srcPath.filename();
    } else if (destExists) {
        errorMessage = L"Cannot move directory: destination is an existing file.";
        return false;
    } else {
        finalDest = destPath;
    }

    // Atomic rename works when both paths share a volume.
    std::error_code renameCode;
    nfs::rename(srcPath, finalDest, renameCode);
    if (!renameCode) {
        return true;
    }

    std::error_code ec;
    nfs::create_directories(finalDest, ec);
    if (ec && !nfs::is_directory(finalDest)) {
        errorMessage = L"Cannot create destination directory.";
        return false;
    }
    for (const auto& dirEnt : nfs::recursive_directory_iterator(srcPath, ec)) {
        if (ec) {
            errorMessage = L"Cannot read source directory.";
            return false;
        }
        const nfs::path& entry = dirEnt.path();
        nfs::path relative = entry.lexically_relative(srcPath);
        nfs::path target = finalDest / relative;
        std::error_code entryCode;
        if (dirEnt.is_directory(entryCode)) {
            nfs::create_directories(target, entryCode);
            if (entryCode && !nfs::is_directory(target)) {
                errorMessage = L"Cannot create destination directory.";
                return false;
            }
        } else {
            nfs::path parent = target.parent_path();
            if (!parent.empty()) {
                nfs::create_directories(parent, entryCode);
            }
            entryCode.clear();
            nfs::copy_file(entry, target, nfs::copy_options::overwrite_existing, entryCode);
            if (entryCode) {
                errorMessage = L"Cannot copy file to destination.";
                return false;
            }
        }
    }

    if (!verifyTreeCopied(srcPath, finalDest, errorMessage)) {
        return false;
    }

    if (!removeSourceDirectory(src, errorMessage)) {
        // The copy is complete, only the removal failed.
        return force;
    }
    return true;
}
//=============================================================================
// Every regular file below src must exist below finalDest with the same size.
static bool
verifyTreeCopied(const nfs::path& src, const nfs::path& finalDest, std::wstring& errorMessage)
{
    std::error_code ec;
    for (const auto& dirEnt : nfs::recursive_directory_iterator(src, ec)) {
        if (ec) {
            errorMessage = L"Cannot read source directory.";
            return false;
        }
        std::error_code entryCode;
        if (!dirEnt.is_regular_file(entryCode)) {
            continue;
        }
        const nfs::path& entry = dirEnt.path();
        nfs::path relative = entry.lexically_relative(src);
        nfs::path target = finalDest / relative;
        if (!nfs::is_regular_file(target, entryCode)) {
            errorMessage = L"Cannot move directory: destination content is incomplete.";
            return false;
        }
        std::error_code sizeSrcCode;
        std::error_code sizeDstCode;
        uintmax_t srcSize = nfs::file_size(entry, sizeSrcCode);
        uintmax_t dstSize = nfs::file_size(target, sizeDstCode);
        if (sizeSrcCode || sizeDstCode || srcSize != dstSize) {
            errorMessage = L"Cannot move directory: destination content is incomplete.";
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
