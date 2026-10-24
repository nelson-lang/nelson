//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcbhBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// gcbh()
//   Get the numeric handle of the block selected in the editor (the block gcb
//   reports as a path). The handle round-trips through get_param/set_param.
//   Returns [] when no block is selected or no editor is active.
//=============================================================================
ArrayOfVector
gcbhBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    std::string blockPath = Nelson::NFlow::NFlowModelRegistry::instance().currentBlock();
    if (blockPath.empty()) {
        retval << ArrayOf::empty();
        return retval;
    }
    double handle = Nelson::NFlow::NFlowModelRegistry::instance().handleForBlock(blockPath);
    retval << ArrayOf::doubleScalar(handle);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
