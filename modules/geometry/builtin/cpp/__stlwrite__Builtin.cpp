//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__stlwrite__Builtin.hpp"
#include "GeometryStlHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::string
toAsciiLower(const std::wstring& value)
{
    std::string out;
    out.reserve(value.size());
    for (wchar_t c : value) {
        out.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(c))));
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GeometryGateway::__stlwrite__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 6, 6);
    std::vector<std::array<double, 3>> points = Stl::arrayToPoints(argIn[1]);
    std::vector<std::array<indexType, 3>> connectivity
        = Stl::arrayToConnectivity(argIn[0], static_cast<indexType>(points.size()));
    std::wstring filename = Stl::textArgument(argIn[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                              L"%s must be a character vector or string scalar."),
            L"File name"));
    std::wstring formatText = Stl::textArgument(argIn[3],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                              L"%s must be a character vector or string scalar."),
            L"File format"));
    std::string fileFormat = toAsciiLower(formatText);
    std::vector<uint16> attributes
        = Stl::arrayToAttributes(argIn[4], static_cast<indexType>(connectivity.size()));
    std::vector<double> solidIds
        = Stl::arrayToSolidIds(argIn[5], static_cast<indexType>(connectivity.size()));
    Stl::writeFile(filename, fileFormat, points, connectivity, attributes, solidIds);
    return {};
}
//=============================================================================
