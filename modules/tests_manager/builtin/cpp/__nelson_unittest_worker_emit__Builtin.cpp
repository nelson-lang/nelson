//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <string>
#if defined(_WIN32)
#include <Windows.h>
#else
#include <cerrno>
#include <unistd.h>
#endif
#include "__nelson_unittest_worker_emit__Builtin.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
writeNativeStdout(const std::string& text)
{
#if defined(_WIN32)
    HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
    if (output == nullptr || output == INVALID_HANDLE_VALUE) {
        return false;
    }
    size_t offset = 0;
    while (offset < text.size()) {
        DWORD written = 0;
        DWORD remaining
            = static_cast<DWORD>((std::min)(text.size() - offset, static_cast<size_t>(MAXDWORD)));
        if (!WriteFile(output, text.data() + offset, remaining, &written, nullptr)
            || written == 0) {
            return false;
        }
        offset += written;
    }
    return true;
#else
    size_t offset = 0;
    while (offset < text.size()) {
        ssize_t written = ::write(STDOUT_FILENO, text.data() + offset, text.size() - offset);
        if (written < 0 && errno == EINTR) {
            continue;
        }
        if (written <= 0) {
            return false;
        }
        offset += static_cast<size_t>(written);
    }
    return true;
#endif
}
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_worker_emit__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!(argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray())) {
        Error(L"Nelson:tests_manager:invalidWorkerPayload",
            L"Worker payload must be a character vector or scalar string.");
    }
    std::string line = "__NELSON_UNITTEST_RESULT__";
    line += wstring_to_utf8(argIn[0].asWideString());
    line.push_back('\n');
    bool success = writeNativeStdout(line);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(success);
    return retval;
}
//=============================================================================
