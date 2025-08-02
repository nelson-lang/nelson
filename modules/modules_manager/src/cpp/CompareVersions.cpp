//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <semver.h>
#include "StringHelpers.hpp"
#include "CompareVersions.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int
CompareVersions(const std::string& versionA, const std::string& versionB, std::string& errorMessage)
{
    semver_t semVersionA = {};
    semver_t semVersionB = {};

    std::string cleanVersionA = versionA;
    if (StringHelpers::starts_with(versionA, "v")) {
        cleanVersionA = cleanVersionA.erase(0, 1);
    }
    std::string cleanVersionB = versionB;
    if (StringHelpers::starts_with(versionB, "v")) {
        cleanVersionB = cleanVersionB.erase(0, 1);
    }

    std::string operatorSatifies;

    if (StringHelpers::starts_with(cleanVersionB, "=")) {
        operatorSatifies = "=";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    }
    if (StringHelpers::starts_with(cleanVersionB, ">=")) {
        operatorSatifies = ">=";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    } else {
        if (StringHelpers::starts_with(cleanVersionB, ">")) {
            operatorSatifies = ">";
            cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
        }
    }
    if (StringHelpers::starts_with(cleanVersionB, "<=")) {
        operatorSatifies = "<=";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    } else {
        if (StringHelpers::starts_with(cleanVersionB, "<")) {
            operatorSatifies = "<";
            cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
        }
    }
    if (StringHelpers::starts_with(cleanVersionB, "^")) {
        operatorSatifies = "^";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    }
    if (StringHelpers::starts_with(cleanVersionB, "~")) {
        operatorSatifies = "~";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    }

    if (semver_parse(cleanVersionA.c_str(), &semVersionA)) {
        errorMessage = _("Invalid value #1");
        return -2;
    }
    if (semver_parse(cleanVersionB.c_str(), &semVersionB)) {
        semver_free(&semVersionA);
        errorMessage = _("Invalid value #2");
        return -2;
    }
    int resolution;
    if (operatorSatifies.empty()) {
        resolution = semver_compare(semVersionA, semVersionB);
    } else {
        resolution = semver_satisfies(semVersionA, semVersionB, operatorSatifies.c_str());
    }
    semver_free(&semVersionA);
    semver_free(&semVersionB);
    return resolution;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
