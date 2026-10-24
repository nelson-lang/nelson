//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timezoneoffsetBuiltin.hpp"
#include "TimeZone.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timezoneoffsetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
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
    // 'local' (default): offsets of wall-clock serial dates; 'utc': of UTC instants.
    bool atUtc = false;
    if (argIn.size() > 3) {
        std::string reference = argIn[3].asUtf8String();
        if (reference != "local" && reference != "utc") {
            Error("Nelson:time:invalidTimeReference", "%s expected.",
                std::string("'local' or 'utc'"));
        }
        atUtc = reference == "utc";
    }

    Dimensions dims = serials.dimensions();
    auto* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, serials.elementCount()));
    const auto* values = static_cast<const double*>(serials.dataPointer());
    for (indexType k = 0; k < serials.elementCount(); ++k) {
        try {
            out[k] = static_cast<double>(atUtc ? TimeZoneOffsetSecondsAtUtc(values[k], timeZoneName)
                                               : TimeZoneOffsetSeconds(values[k], timeZoneName));
        } catch (const std::exception&) {
            Error("Nelson:time:invalidTimeZone", "Invalid %s.", std::string("time zone"));
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, dims, out);
    return retval;
}
//=============================================================================
