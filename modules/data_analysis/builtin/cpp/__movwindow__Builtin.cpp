//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__movwindow__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "MovingWindow.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static indexType
nonnegativeIntegerArgument(const ArrayOfVector& argIn, int position);
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::__movwindow__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    // y = __movwindow__(x, before, after, firstRow, lastRow, omitnan, operation)
    // sliding statistic of the columns of x, operation is one of
    // 'median', 'mad', 'sum', 'mean', 'min', 'max'; see MovingWindow.
    ArrayOfVector retval;
    nargincheck(argIn, 7, 7);
    nargoutcheck(nLhs, 0, 1);
    indexType before = nonnegativeIntegerArgument(argIn, 1);
    indexType after = nonnegativeIntegerArgument(argIn, 2);
    indexType firstRow = nonnegativeIntegerArgument(argIn, 3);
    indexType lastRow = nonnegativeIntegerArgument(argIn, 4);
    bool omitNaN = argIn[5].asLogicalScalar();
    MovingWindowOperation operation;
    if (!argIn[6].isRowVectorCharacterArray() && !argIn[6].isScalarStringArray()) {
        Error("Nelson:data_analysis:unsupportedOperation", "Unsupported operation.");
    }
    if (!MovingWindowOperationFromName(argIn[6].asWideString(), operation)) {
        Error("Nelson:data_analysis:unsupportedOperation", "Unsupported operation.");
    }
    bool needToOverload = false;
    ArrayOf res = MovingWindow(
        argIn[0], before, after, firstRow, lastRow, omitNaN, operation, needToOverload);
    if (needToOverload) {
        OverloadRequired("__movwindow__");
    }
    retval << res;
    return retval;
}
//=============================================================================
static indexType
nonnegativeIntegerArgument(const ArrayOfVector& argIn, int position)
{
    int64 value = argIn[position].asInt64Scalar(false, true);
    if (value < 0) {
        Error("Nelson:data_analysis:windowArgumentNonnegative",
            "Window arguments must be nonnegative integers.");
    }
    return (indexType)value;
}
//=============================================================================
