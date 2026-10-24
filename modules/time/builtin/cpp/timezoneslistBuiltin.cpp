//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timezoneslistBuiltin.hpp"
#include "TimeZone.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timezoneslistBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn.empty()) {
        TimeZoneSetDataPath(argIn[0].asUtf8String());
    }
    stringVector zones = TimeZoneList();
    Dimensions dims(zones.size(), 1);
    ArrayOfVector retval;
    retval << ArrayOf::stringArray(zones, dims);
    if (nLhs > 1) {
        retval << ArrayOf::stringArray(TimeZoneDataVersion());
    }
    return retval;
}
//=============================================================================
