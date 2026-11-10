//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <ctime>
#include "matioHelpers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Dimensions
getMatVarDimensions(matvar_t* matVariable)
{
    Dimensions dims;
    if (matVariable) {
        for (int d = 0; d < matVariable->rank; d++) {
            dims[d] = matVariable->dims[d];
        }
    }
    return dims;
}
//=============================================================================
size_t*
convertDimensionsForMatVar(const Dimensions& variableDims, indexType& rank)
{
    rank = variableDims.length();
    size_t* dims = nullptr;
    try {
        dims = new size_t[rank];
    } catch (const std::bad_alloc&) {
        rank = 0;
        return nullptr;
    }
    Dimensions _dims(variableDims);
    for (indexType k = 0; k < rank; k++) {
        dims[k] = _dims[k];
    }
    return dims;
}
//=============================================================================
std::string
createHeaderMatioFile()
{
    std::string header = std::string("Nelson 1.0 MAT-file");
    header = header + std::string(" Created by libmatio ") + std::string(MATIO_VERSION_STR);
#ifdef _MSC_VER
    time_t _tm = time(nullptr);
    struct tm* curtime = localtime(&_tm);
    std::string timestr = asctime(curtime);
#else
    struct tm newtime;
    time_t ltime;
    char buf[128];
    ltime = time(&ltime);
    localtime_r(&ltime, &newtime);
    std::string timestr = asctime_r(&newtime, buf);
#endif
    StringHelpers::replace_last(timestr, "\n", "");
    header = header + std::string(" on ") + timestr;
    return header;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
