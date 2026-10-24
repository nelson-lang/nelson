//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "canonicalGraphicsPropertyBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "IsValidGraphicsProperty.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// canonicalGraphicsProperty(type, name) -> canonical property name for the
// graphics object type, resolving the shortest unambiguous leading substring
// (e.g. 'FaceC' -> 'FaceColor'); returns '' when the name is not a recognised
// property. Lets the .m plotting wrappers (patch/surf/fill/trisurf) share the
// one C++ resolution instead of duplicating hard-coded property-name lists.
//=============================================================================
ArrayOfVector
GraphicsGateway::canonicalGraphicsPropertyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    std::wstring GOTypename = argIn[0].asWideString();
    std::wstring GOPropertyName = argIn[1].asWideString();

    std::wstring canonical = CanonicalGraphicsProperty(GOTypename, GOPropertyName);

    retval << ArrayOf::characterArray(canonical);
    return retval;
}
//=============================================================================
