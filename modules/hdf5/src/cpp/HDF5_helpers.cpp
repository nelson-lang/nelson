//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "HDF5_helpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void* hdf5ErrorFunction;
static H5E_auto_t oef;
//=============================================================================
void
disableHdf5Warning()
{
    H5Eget_auto(H5E_DEFAULT, &oef, &hdf5ErrorFunction);
    H5Eset_auto(H5E_DEFAULT, nullptr, nullptr);
}
//=============================================================================
void
enableHdf5Warning()
{
    H5Eset_auto(H5E_DEFAULT, oef, hdf5ErrorFunction);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
