//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bdIsDirtyBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
ArrayOfVector
bdIsDirtyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(doc.dirty());
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
