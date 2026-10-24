//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bdIsLoadedBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
ArrayOfVector
bdIsLoadedBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool loaded = false;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        loaded = NFlowModelRegistry::instance().isLoaded(argIn[0].asUtf8String());
    } else if (argIn[0].isNumeric() && !argIn[0].isEmpty()) {
        loaded
            = NFlowModelRegistry::instance().resolveByHandle(argIn[0].asDoubleScalar()) != nullptr;
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(loaded);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
