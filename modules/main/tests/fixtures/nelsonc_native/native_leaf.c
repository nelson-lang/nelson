//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#ifdef _WIN32
__declspec(dllexport)
#endif
double
nc_native_leaf(double value)
{
    return 2 * value;
}
