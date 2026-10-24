//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "h5writeBuiltin.hpp"
#include "h5writeattBuiltin.hpp"
#include "h5readattBuiltin.hpp"
#include "h5createBuiltin.hpp"
#include "h5readBuiltin.hpp"
#include "h5saveBuiltin.hpp"
#include "h5loadBuiltin.hpp"
#include "isnh5fileBuiltin.hpp"
#include "whosnh5Builtin.hpp"
#include "whonh5Builtin.hpp"
#include "h5lsBuiltin.hpp"
#include "h5dumpBuiltin.hpp"
#include "HDF5_helpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"hdf5";
//=============================================================================
static const nlsGateway gateway[] = {
    { "whosnh5", (ptrBuiltin)Nelson::Hdf5Gateway::whosnh5Builtin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "whonh5", (ptrBuiltin)Nelson::Hdf5Gateway::whonh5Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "loadnh5", (ptrBuiltin)Nelson::Hdf5Gateway::h5loadBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "savenh5", (ptrBuiltin)Nelson::Hdf5Gateway::h5saveBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "h5write", (ptrBuiltin)Nelson::Hdf5Gateway::h5writeBuiltin, 0, 3 },
    { "h5writeatt", (ptrBuiltin)Nelson::Hdf5Gateway::h5writeattBuiltin, 0, -1 },
    { "h5readatt", (ptrBuiltin)Nelson::Hdf5Gateway::h5readattBuiltin, 1, 3 },
    { "h5read", (ptrBuiltin)Nelson::Hdf5Gateway::h5readBuiltin, 1, 2 },
    { "h5ls", (ptrBuiltin)Nelson::Hdf5Gateway::h5lsBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "h5dump", (ptrBuiltin)Nelson::Hdf5Gateway::h5dumpBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "h5create", (ptrBuiltin)Nelson::Hdf5Gateway::h5createBuiltin, 0, -4 },
    { "isnh5file", (ptrBuiltin)Nelson::Hdf5Gateway::isnh5fileBuiltin, 1, 1 },
};
//=============================================================================
static bool
initializeHdf5Module(Nelson::Evaluator* eval)
{
    disableHdf5Warning();
    BuiltInFunctionDefManager::getInstance()->setCallerContextAccess(
        (ptrBuiltin)Nelson::Hdf5Gateway::h5loadBuiltin, true);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, (void*)initializeHdf5Module)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
