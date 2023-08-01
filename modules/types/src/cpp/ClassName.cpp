//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassName.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "ClassToString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
ClassName(const ArrayOf& In)
{
    std::string classString = {};
    if (In.isSparse()) {
        return std::string(NLS_SPARSE_STR) + ClassToString(In.getDataClass());
    }
    switch (In.getDataClass()) {
    case NLS_HANDLE: {
        classString = NLS_HANDLE_STR;
        /* handle can be 'handle' or another type but not mixed */
        auto* qp = (nelson_handle*)In.getDataPointer();
        if (qp) {
            indexType elementCount = In.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr) {
                    std::string current = wstring_to_utf8(hlObj->getCategory());
                    if (classString != current && current != NLS_HANDLE_STR) {
                        classString = std::move(current);
                    }
                }
            }
        }
        return classString;
    } break;
    case NLS_CLASS_ARRAY: {
        classString = In.getClassType();
    } break;
    default: {
        classString = ClassToString(In.getDataClass());
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
