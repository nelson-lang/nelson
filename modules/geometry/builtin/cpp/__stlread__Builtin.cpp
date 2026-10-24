//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__stlread__Builtin.hpp"
#include "GeometryStlHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GeometryGateway::__stlread__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 5);
    nargincheck(argIn, 1, 1);
    std::wstring filename = Stl::textArgument(argIn[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                              L"%s must be a character vector or string scalar."),
            L"File name"));
    Stl::Mesh mesh = Stl::readFile(filename);
    ArrayOfVector retval;
    retval << Stl::connectivityToArray(mesh.connectivity);
    if (nLhs > 1) {
        retval << Stl::pointsToArray(mesh.points);
    }
    if (nLhs > 2) {
        retval << ArrayOf::characterArray(mesh.binary ? "binary" : "text");
    }
    if (nLhs > 3) {
        retval << Stl::attributesToArray(
            mesh.attributes, mesh.binary, static_cast<indexType>(mesh.connectivity.size()));
    }
    if (nLhs > 4) {
        retval << Stl::solidIdsToArray(mesh.solidIds);
    }
    return retval;
}
//=============================================================================
