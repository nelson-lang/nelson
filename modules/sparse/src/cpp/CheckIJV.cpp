//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CheckIJV.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
CheckIJV(
    size_t ilen, size_t jlen, size_t vlen, int& istride, int& jstride, int& vstride, size_t& olen)
{
    olen = 0;
    olen = ilen > jlen ? ilen : jlen;
    olen = vlen > olen ? vlen : olen;
    istride = 0;
    jstride = 0;
    vstride = 0;
    if (olen > 1) {
        if (ilen == 1) {
            istride = 0;
        } else if (ilen == olen) {
            istride = 1;
        } else {
            raiseError(L"Nelson:sparse:ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS",
                ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS);
        }
        if (jlen == 1) {
            jstride = 0;
        } else if (jlen == olen) {
            jstride = 1;
        } else {
            raiseError(L"Nelson:sparse:ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS",
                ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS);
        }
        if (vlen == 1) {
            vstride = 0;
        } else if (vlen == olen) {
            vstride = 1;
        } else {
            raiseError(L"Nelson:sparse:ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS",
                ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS);
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
