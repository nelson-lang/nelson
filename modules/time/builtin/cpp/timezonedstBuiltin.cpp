//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timezonedstBuiltin.hpp"
#include "TimeZone.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timezonedstBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isNumeric()) {
        Error("Nelson:time:numericDateNumberExpected", "%s expected.",
            std::string("Numeric date number"));
    }
    ArrayOf serials = argIn[0];
    serials.promoteType(NLS_DOUBLE);
    std::string timeZoneName = argIn[1].asUtf8String();
    if (argIn.size() > 2) {
        TimeZoneSetDataPath(argIn[2].asUtf8String());
    }
    // 'local' (default) or 'first': occurrence read for an ambiguous wall-clock time.
    bool firstOccurrence = false;
    if (argIn.size() > 3) {
        std::string reference = argIn[3].asUtf8String();
        if (reference != "local" && reference != "first") {
            Error("Nelson:time:invalidTimeReference", "%s expected.",
                std::string("'local' or 'first'"));
        }
        firstOccurrence = reference == "first";
    }

    Dimensions dims = serials.dimensions();
    auto* out
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, serials.elementCount()));
    const auto* values = static_cast<const double*>(serials.dataPointer());
    // second output: daylight saving shift in seconds.
    double* shift = nullptr;
    if (nLhs > 1) {
        shift = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, serials.elementCount()));
    }
    for (indexType k = 0; k < serials.elementCount(); ++k) {
        try {
            int dstSeconds = TimeZoneDstSeconds(values[k], timeZoneName, firstOccurrence);
            out[k] = dstSeconds != 0;
            if (shift != nullptr) {
                shift[k] = static_cast<double>(dstSeconds);
            }
        } catch (const std::exception&) {
            Error("Nelson:time:invalidTimeZone", "Invalid %s.", std::string("time zone"));
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_LOGICAL, dims, out);
    if (shift != nullptr) {
        retval << ArrayOf(NLS_DOUBLE, dims, shift);
    }
    return retval;
}
//=============================================================================
