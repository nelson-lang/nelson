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
    // 'local' (default): offsets of wall-clock serial dates; 'first': same, with the
    // first occurrence of an ambiguous time; 'utc': offsets of UTC instants.
    bool atUtc = false;
    bool firstOccurrence = false;
    if (argIn.size() > 3) {
        std::string reference = argIn[3].asUtf8String();
        if (reference != "local" && reference != "utc" && reference != "first") {
            Error("Nelson:time:invalidTimeReference", "%s expected.",
                std::string("'local', 'first' or 'utc'"));
        }
        atUtc = reference == "utc";
        firstOccurrence = reference == "first";
    }

    Dimensions dims = serials.dimensions();
    auto* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, serials.elementCount()));
    const auto* values = static_cast<const double*>(serials.dataPointer());
    // second output: ambiguous wall times ('local', 'first') or instants whose
    // wall time comes again after the next DST end ('utc').
    logical* repeated = nullptr;
    if (nLhs > 1) {
        repeated
            = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, serials.elementCount()));
    }
    for (indexType k = 0; k < serials.elementCount(); ++k) {
        try {
            bool flag = false;
            bool* flagPtr = repeated != nullptr ? &flag : nullptr;
            out[k] = static_cast<double>(atUtc
                    ? TimeZoneOffsetSecondsAtUtc(values[k], timeZoneName, flagPtr)
                    : TimeZoneOffsetSeconds(values[k], timeZoneName, firstOccurrence, flagPtr));
            if (repeated != nullptr) {
                repeated[k] = flag;
            }
        } catch (const std::exception&) {
            Error("Nelson:time:invalidTimeZone", "Invalid %s.", std::string("time zone"));
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, dims, out);
    if (repeated != nullptr) {
        retval << ArrayOf(NLS_LOGICAL, dims, repeated);
    }
    return retval;
}
//=============================================================================
