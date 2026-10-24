//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getNFlowBlockHandleBuiltin.hpp"
#include "load_systemBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "NFlowModelDocument.hpp"
#include "Dimensions.hpp"
#include "NelsonException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <vector>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// Resolve one 'model/block' path to a handle, or -1 when it is not a loaded
// block. When load is true and the model is not loaded, a best-effort load is
// attempted (errors are swallowed, keeping the no-error contract).
//=============================================================================
static double
resolveOnePath(const std::string& path, bool load)
{
    auto pos = path.find('/');
    // A path with no '/' names a model, not a block: not a valid block handle.
    if (pos == std::string::npos || pos + 1 >= path.size()) {
        return -1.0;
    }
    std::string modelName = path.substr(0, pos);
    std::string blockName = path.substr(pos + 1);
    auto& reg = NFlowModelRegistry::instance();
    NFlowModelDocument* doc = reg.resolveByName(modelName);
    if (doc == nullptr && load) {
        try {
            ArrayOfVector a;
            a << ArrayOf::characterArray(modelName);
            load_systemBuiltin(0, a);
        } catch (const NelsonException&) {
            // Model could not be loaded: leave it unresolved (returns -1).
        }
        doc = reg.resolveByName(modelName);
    }
    if (doc == nullptr || !doc->hasBlock(blockName)) {
        return -1.0;
    }
    return reg.handleForBlock(modelName + "/" + blockName);
}
//=============================================================================
// getNFlowBlockHandle(path)
// getNFlowBlockHandle(path, load)
//   Return the numeric handle of a block by path, or -1 when the block is not
//   found (no error). path is a char row vector or a cell array of char row
//   vectors; a cell input yields a double array of the same shape. When load is
//   true, an unloaded model is loaded first if possible.
//=============================================================================
ArrayOfVector
getNFlowBlockHandleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool load = false;
    if (argIn.size() == 2) {
        load = argIn[1].isLogical() ? argIn[1].asLogicalScalar()
                                    : (argIn[1].asDoubleScalar() != 0.0);
    }
    ArrayOfVector retval;
    const ArrayOf& arg = argIn[0];
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        retval << ArrayOf::doubleScalar(resolveOnePath(arg.asUtf8String(), load));
        return retval;
    }
    if (arg.isCell()) {
        const ArrayOf* elements = static_cast<const ArrayOf*>(arg.dataPointer());
        indexType n = arg.elementCount();
        double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
        for (indexType i = 0; i < n; ++i) {
            const ArrayOf& e = elements[i];
            if (!(e.isRowVectorCharacterArray() || e.isScalarStringArray())) {
                Error("Nelson:nflow_engine:invalidArgument:eachCellContainABlockPath",
                    "Each cell must contain a block path (char row vector).");
            }
            out[i] = resolveOnePath(e.asUtf8String(), load);
        }
        retval << ArrayOf(NLS_DOUBLE, arg.dimensions(), out);
        return retval;
    }
    // A numeric argument is not a path: it cannot name a block, so it resolves
    // to -1 (element-wise), never an error.
    if (arg.isNumeric() && !arg.isEmpty()) {
        indexType n = arg.elementCount();
        double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
        for (indexType i = 0; i < n; ++i) {
            out[i] = -1.0;
        }
        retval << ArrayOf(NLS_DOUBLE, arg.dimensions(), out);
        return retval;
    }
    Error("Nelson:nflow_engine:invalidArgument:aBlockPathCharACell",
        "A block path (char) or a cell array of block paths is expected.");
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
