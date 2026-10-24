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
    nargincheck(argIn, 2, 3);
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

    Dimensions dims = serials.dimensions();
    auto* out
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, serials.elementCount()));
    const auto* values = static_cast<const double*>(serials.dataPointer());
    for (indexType k = 0; k < serials.elementCount(); ++k) {
        try {
            out[k] = TimeZoneIsDst(values[k], timeZoneName);
        } catch (const std::exception&) {
            Error("Nelson:time:invalidTimeZone", "Invalid %s.", std::string("time zone"));
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_LOGICAL, dims, out);
    return retval;
}
//=============================================================================
