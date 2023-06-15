//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UnaryPlus.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
UnaryPlus(const ArrayOf& A)
{
    ArrayOf res;
    switch (A.getDataClass()) {
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = A;
        res.promoteType(NLS_DOUBLE);
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        res = A;
    } break;
    default: {
        OverloadRequired("uplus");
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
