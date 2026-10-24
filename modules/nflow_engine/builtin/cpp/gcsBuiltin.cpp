//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcsBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// gcs()
//   Get the current system: the active model/subsystem path in the editor, or
//   the last system created/loaded/opened by a script. Returns '' when no system
//   is active (the last model was closed, or nothing is loaded).
//=============================================================================
ArrayOfVector
gcsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(
        Nelson::NFlow::NFlowModelRegistry::instance().currentSystem());
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
