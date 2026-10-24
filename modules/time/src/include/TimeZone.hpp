//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTIME_IMPEXP void
TimeZoneSetDataPath(const std::string& dataPath);
//=============================================================================
// UTC offset of a wall-clock serial date number: a time skipped by a DST gap
// takes the offset before the gap, an ambiguous time the later occurrence.
NLSTIME_IMPEXP int
TimeZoneOffsetSeconds(double serialDateNumber, const std::string& timeZoneName);
//=============================================================================
// UTC offset at an instant given as a UTC serial date number.
NLSTIME_IMPEXP int
TimeZoneOffsetSecondsAtUtc(double utcSerialDateNumber, const std::string& timeZoneName);
//=============================================================================
// Canonical name of a time zone: '' stays '', 'UTC'/'GMT' give 'UTC', 'Z' and
// '+HHMM' give '+HH:MM', 'local' the system time zone, a link its target zone.
// Throws std::invalid_argument for an unknown time zone.
NLSTIME_IMPEXP std::string
TimeZoneCanonicalName(const std::string& timeZoneName);
//=============================================================================
NLSTIME_IMPEXP bool
TimeZoneIsDst(double serialDateNumber, const std::string& timeZoneName);
//=============================================================================
NLSTIME_IMPEXP std::vector<std::string>
TimeZoneList();
//=============================================================================
NLSTIME_IMPEXP std::string
TimeZoneDataVersion();
//=============================================================================
} // namespace Nelson
//=============================================================================
