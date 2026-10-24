//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__islocalextrema__Builtin.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "LocalExtrema.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static LocalExtremaFlatSelection
flatSelectionArgument(const ArrayOf& arg);
static void
windowArgument(const ArrayOf& arg, LocalExtremaOptions& options);
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::__islocalextrema__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    // [tf, P] = __islocalextrema__(A, isMaximum, dim, flatSelection, minProminence,
    //                              minSeparation, maxNumExtrema, samplePoints, window)
    // arguments are validated by islocalmax and islocalmin; see LocalExtrema.
    ArrayOfVector retval;
    nargincheck(argIn, 9, 9);
    nargoutcheck(nLhs, 0, 2);
    LocalExtremaOptions options;
    options.isMaximum = argIn[1].asLogicalScalar();
    indexType dim = (indexType)argIn[2].asDoubleScalar(false, true);
    if (dim < 1) {
        Error(L"Nelson:isLocalExtrema:DimensionInvalid", L"Dimension must be a positive integer.");
    }
    options.flatSelection = flatSelectionArgument(argIn[3]);
    options.minProminence = argIn[4].asDoubleScalar();
    options.minSeparation = argIn[5].asDoubleScalar();
    options.maxNumExtrema = argIn[6].asDoubleScalar();
    if (!argIn[7].isEmpty()) {
        options.samplePoints = argIn[7].asDoubleVector();
    }
    windowArgument(argIn[8], options);
    options.wantProminence = nLhs > 1;
    ArrayOf tf;
    ArrayOf prominence;
    LocalExtrema(argIn[0], dim, options, tf, prominence);
    retval << tf;
    if (nLhs > 1) {
        retval << prominence;
    }
    return retval;
}
//=============================================================================
static LocalExtremaFlatSelection
flatSelectionArgument(const ArrayOf& arg)
{
    std::wstring name = arg.asWideString();
    if (name == L"center") {
        return LocalExtremaFlatSelection::Center;
    }
    if (name == L"first") {
        return LocalExtremaFlatSelection::First;
    }
    if (name == L"last") {
        return LocalExtremaFlatSelection::Last;
    }
    if (name == L"all") {
        return LocalExtremaFlatSelection::All;
    }
    Error(L"Nelson:isLocalExtrema:FlatSelectionInvalid",
        L"'FlatSelection' must be 'first', 'center', 'last', or 'all'.");
    return LocalExtremaFlatSelection::Center;
}
//=============================================================================
// empty: no window, scalar k: [x - k/2, x + k/2), [b f]: [x - b, x + f].
static void
windowArgument(const ArrayOf& arg, LocalExtremaOptions& options)
{
    if (arg.isEmpty()) {
        return;
    }
    std::vector<double> window = arg.asDoubleVector();
    if (window.size() == 1) {
        options.windowBefore = window[0] / 2;
        options.windowAfter = window[0] / 2;
        options.windowRightOpen = true;
    } else if (window.size() == 2) {
        options.windowBefore = window[0];
        options.windowAfter = window[1];
    } else {
        Error(L"Nelson:isLocalExtrema:ProminenceWindowLengthInvalid",
            L"'ProminenceWindow' value must be a positive scalar or 2-element vector of "
            L"nonnegative scalars.");
    }
    options.hasWindow = true;
}
//=============================================================================
