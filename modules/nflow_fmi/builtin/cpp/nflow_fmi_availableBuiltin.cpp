//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_fmi_availableBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FmiPackage.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::NFlowFmiGateway::nflow_fmi_availableBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(Nelson::NFlow::Fmi::fmiSupportCompiledIn());
    return retval;
}
//=============================================================================
