//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include "ioBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileOpen.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileArchiverGateway {
//=============================================================================
uint64_t
sizeLimit(const ArrayOf& value)
{
    const double limit = value.asDoubleScalar();
    if (std::isinf(limit) && limit > 0) {
        return std::numeric_limits<uint64_t>::max();
    }
    if (!(limit >= 0) || limit != std::floor(limit)) {
        Error(L"Nelson:io:invalidMaxSize", L"MaxSize must be Inf or a nonnegative integer.");
    }
    return static_cast<uint64_t>(limit);
}
//=============================================================================
ExtractionLimits
extractionLimits(const ArrayOf& maxExtractedSize, const ArrayOf& maxCompressionRatio)
{
    ExtractionLimits limits;
    const double size = maxExtractedSize.asDoubleScalar();
    if (!(std::isinf(size) && size > 0)) {
        if (!(size >= 0) || size != std::floor(size)) {
            Error(L"Nelson:file_archiver:invalidMaxExtractedSize",
                L"MaxExtractedSize must be Inf or a nonnegative integer.");
        }
        limits.maxTotalSize = static_cast<uint64_t>(size);
    }
    const double ratio = maxCompressionRatio.asDoubleScalar();
    if (!(ratio > 0)) {
        Error(L"Nelson:file_archiver:invalidMaxCompressionRatio",
            L"MaxCompressionRatio must be Inf or a positive number.");
    }
    limits.maxRatio = std::isinf(ratio) ? 0.0 : ratio;
    return limits;
}
//=============================================================================
wchar_t
fileMode(const ArrayOf& mode, bool allowAppend)
{
    const std::wstring value = mode.asWideString();
    const bool valid = (value.size() == 1 || (value.size() == 2 && value[1] == L'b'))
        && (value[0] == L'r' || value[0] == L'w' || (allowAppend && value[0] == L'a'));
    if (!valid) {
        if (allowAppend) {
            Error(L"Nelson:io:invalidAppendFileMode", L"File mode must be 'r', 'w' or 'a'.");
        }
        Error(L"Nelson:io:invalidFileMode", L"File mode must be 'r' or 'w'.");
    }
    return value[0];
}
//=============================================================================
std::wstring
checkedEncoding(const ArrayOf& encoding)
{
    const std::wstring value = encoding.asWideString();
    if (!isSupportedEncoding(value)) {
        Error(L"Nelson:stream_manager:invalidEncodingArgument", L"Invalid encoding.");
    }
    return value;
}
//=============================================================================
ArrayOf
openFileIdentifier(
    FileBackend* backend, const std::wstring& name, wchar_t mode, const std::wstring& encoding)
{
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    const int fid = FileOpenBackend(fm, backend, name, std::wstring(1, mode) + L"b", encoding);
    if (fid < 0) {
        Error(L"Nelson:stream_manager:impossibleAddFile", L"Impossible to add file.");
    }
    return ArrayOf::doubleScalar(static_cast<double>(fid));
}
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
