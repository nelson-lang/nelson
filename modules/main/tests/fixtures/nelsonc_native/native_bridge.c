//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#ifdef _WIN32
__declspec(dllimport)
#endif
double
nc_native_leaf(double value);

#ifdef _WIN32
__declspec(dllexport)
#endif
double
nc_native_bridge(double value)
{
    return nc_native_leaf(value) + 3;
}
