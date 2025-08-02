//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOColorVectorProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOColorVectorProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    arg.promoteType(NLS_DOUBLE);
    bool isValid = arg.isEmpty() || (arg.is2D() && (arg.getDimensionLength(1) == 3));
    if (!isValid) {
        Error(_("Expect an m x 3 matrix for color orders."));
    }
    const double* dp = static_cast<const double*>(
        const_cast<void*>(static_cast<const void*>(arg.getDataPointer())));

    indexType n = arg.getElementCount();
    for (indexType i = 0; i < n; i++) {
        if ((dp[i] < 0) || (dp[i] > 1.0)) {
            Error(_("Color vector must be between 0 and 1."));
        }
    }
    _data.clear();
    indexType rows = n / 3;
    _data.reserve(rows * 3);
    for (indexType i = 0; i < rows; i++) {
        for (indexType j = 0; j < 3; j++) {
            _data.push_back(dp[i + j * rows]);
        }
    }
}
//=============================================================================
ArrayOf
GOColorVectorProperty::get()
{
    indexType count = _data.size();
    indexType rows = count / 3;
    double* rp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, count);
    for (indexType i = 0; i < rows; i++) {
        for (indexType j = 0; j < 3; j++) {
            rp[i + j * rows] = _data[3 * i + j];
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(rows, 3), rp);
}
//=============================================================================
}
//=============================================================================
