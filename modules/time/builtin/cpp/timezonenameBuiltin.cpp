//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timezonenameBuiltin.hpp"
#include "TimeZone.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timezonenameBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::string timeZoneName = argIn[0].asUtf8String();
    if (argIn.size() > 1) {
        TimeZoneSetDataPath(argIn[1].asUtf8String());
    }
    std::string canonicalName;
    try {
        canonicalName = TimeZoneCanonicalName(timeZoneName);
    } catch (const std::exception&) {
        Error("Nelson:datetime:UnknownTimeZone",
            "Unable to create the specified time zone: '%s'. See the datetime.TimeZone property "
            "for details about specifying time zones.",
            timeZoneName);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(canonicalName);
    return retval;
}
//=============================================================================
