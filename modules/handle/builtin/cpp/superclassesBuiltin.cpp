//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <functional>
#include <unordered_set>
#include "superclassesBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// superclasses(CLASSNAME) / superclasses(OBJECT): names of all visible
// superclasses, nearest ancestor first, as an N-by-1 cell array of char.
ArrayOfVector
Nelson::HandleGateway::superclassesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string className;
    if (argIn[0].isClassType()) {
        className = argIn[0].classType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].handleClassName();
    } else if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        className = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (!classdefLoadClass(className)) {
        RaiseError(
            resolveMessageTemplate("Nelson:handle:classFound", "Class not found: ") + className,
            "Nelson:superclasses:classNotFound");
    }
    stringVector names;
    std::unordered_set<std::string> seen;
    std::function<void(const std::string&)> collect = [&](const std::string& current) {
        for (const auto& superclass : classdefSuperclasses(current)) {
            if (superclass.empty() || !seen.insert(superclass).second) {
                continue;
            }
            names.push_back(superclass);
            if (classdefLoadClass(superclass)) {
                collect(superclass);
            }
        }
    };
    collect(className);
    ArrayOfMatrix matrix;
    matrix.reserve(names.size());
    for (const auto& name : names) {
        ArrayOfVector row;
        row << ArrayOf::characterArray(name);
        matrix.push_back(row);
    }
    retval << ArrayOf::cellArray(matrix);
    return retval;
}
//=============================================================================
