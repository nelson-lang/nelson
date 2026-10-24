//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassName.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "characters_encoding.hpp"
#include "ClassToString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
ClassName(const ArrayOf& In)
{
    std::string classString = {};
    switch (In.dataClass()) {
    case NLS_HANDLE: {
        classString = NLS_HANDLE_STR;
        auto* qp = (HandleRef*)In.dataPointer();
        if (In.elementCount() == 0) {
            const std::string handleClassName = In.handleClassName();
            return handleClassName.empty() ? classString : handleClassName;
        }
        if (qp) {
            indexType elementCount = In.elementCount();
            for (indexType k = 0; k < elementCount; k++) {
                if (qp[k]) {
                    std::string current = qp[k]->category();
                    if (classString != current && current != NLS_HANDLE_STR) {
                        classString = std::move(current);
                    }
                }
            }
        }
        return classString;
    } break;
    case NLS_FUNCTION_HANDLE: {
        classString = NLS_FUNCTION_HANDLE_STR;
    } break;
    case NLS_GO_HANDLE: {
        classString = In.graphicsObjectClassName();
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        classString = In.classType();
    } break;
    default: {
        if (In.isSparse()) {
            return std::string(NLS_SPARSE_STR) + ClassToString(In.dataClass());
        }
        classString = ClassToString(In.dataClass());
    } break;
    }
    return classString;
}
//=============================================================================
stringVector
ClassName(const ArrayOfVector& In)
{
    stringVector strs;
    for (const auto& k : In) {
        strs.push_back(ClassName(k));
    }
    return strs;
}
//=============================================================================
void
ClassName(const ArrayOf& In, std::wstring& returnedClassName)
{
    returnedClassName = utf8_to_wstring(ClassName(In));
}
//=============================================================================
void
ClassName(const ArrayOf& In, std::string& returnedClassName)
{
    returnedClassName = ClassName(In);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
