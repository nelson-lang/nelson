//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getfullnameBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// A block handle gives 'model/block', a model handle gives 'model'. Raises on an unknown handle.
//=============================================================================
static std::string
handleToFullName(double handle)
{
    auto& reg = NFlowModelRegistry::instance();
    std::string path;
    if (reg.resolveBlockHandle(handle, path)) {
        return path;
    }
    if (reg.nameOfHandle(handle, path)) {
        return path;
    }
    Error("Nelson:nflow_engine:invalidHandle:invalidNflowObjectHandle",
        "Invalid nflow object handle.");
    return path;
}
//=============================================================================
// getfullname(handle)
//   Return the full path of the block or model named by a handle: a block
//   handle yields 'model/block', a model handle yields 'model'. A char path is
//   returned unchanged. A cell array of handles yields a cell array of paths of
//   the same shape.
//=============================================================================
ArrayOfVector
getfullnameBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    const ArrayOf& arg = argIn[0];
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        retval << ArrayOf::characterArray(arg.asUtf8String());
        return retval;
    }
    if (arg.isCell()) {
        const ArrayOf* elements = static_cast<const ArrayOf*>(arg.dataPointer());
        indexType n = arg.elementCount();
        ArrayOf* out = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n));
        for (indexType i = 0; i < n; ++i) {
            const ArrayOf& e = elements[i];
            if (!e.isNumeric() || e.isEmpty()) {
                Error("Nelson:nflow_engine:invalidArgument",
                    "Each cell must contain a numeric handle.");
            }
            out[i] = ArrayOf::characterArray(handleToFullName(e.asDoubleScalar()));
        }
        retval << ArrayOf(NLS_CELL_ARRAY, arg.dimensions(), out);
        return retval;
    }
    if (arg.isNumeric() && !arg.isEmpty()) {
        if (arg.isScalar()) {
            retval << ArrayOf::characterArray(handleToFullName(arg.asDoubleScalar()));
            return retval;
        }
        ArrayOf d(arg);
        d.promoteType(NLS_DOUBLE);
        const double* p = static_cast<const double*>(d.dataPointer());
        indexType n = d.elementCount();
        ArrayOf* out = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n));
        for (indexType i = 0; i < n; ++i) {
            out[i] = ArrayOf::characterArray(handleToFullName(p[i]));
        }
        retval << ArrayOf(NLS_CELL_ARRAY, arg.dimensions(), out);
        return retval;
    }
    Error("Nelson:nflow_engine:invalidHandle:aHandleACellArrayHandles",
        "A handle or a cell array of handles is expected.");
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
