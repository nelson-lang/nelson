//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <semver.h>
#include <boost/algorithm/string.hpp>
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
    if (boost::algorithm::starts_with(versionA, "v")) {
        cleanVersionA = cleanVersionA.erase(0, 1);
    }
    std::string cleanVersionB = versionB;
    if (boost::algorithm::starts_with(versionB, "v")) {
        cleanVersionB = cleanVersionB.erase(0, 1);
    }

    std::string operatorSatifies;

    if (boost::algorithm::starts_with(cleanVersionB, "=")) {
        operatorSatifies = "=";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    }
    if (boost::algorithm::starts_with(cleanVersionB, ">=")) {
        operatorSatifies = ">=";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    } else {
        if (boost::algorithm::starts_with(cleanVersionB, ">")) {
            operatorSatifies = ">";
            cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
        }
    }
    if (boost::algorithm::starts_with(cleanVersionB, "<=")) {
        operatorSatifies = "<=";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    } else {
        if (boost::algorithm::starts_with(cleanVersionB, "<")) {
            operatorSatifies = "<";
            cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
        }
    }
    if (boost::algorithm::starts_with(cleanVersionB, "^")) {
        operatorSatifies = "^";
        cleanVersionB = cleanVersionB.erase(0, operatorSatifies.size());
    }
    if (boost::algorithm::starts_with(cleanVersionB, "~")) {
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
