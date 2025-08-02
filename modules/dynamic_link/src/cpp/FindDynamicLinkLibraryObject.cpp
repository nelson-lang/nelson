//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "FindDynamicLinkLibraryObject.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
findDynamicLinkLibraryObject(const std::wstring& libraryName, int nLhs)
{
    ArrayOfVector retval;

    std::vector<nelson_handle> used
        = HandleManager::getInstance()->getAllHandlesOfCategory(NLS_HANDLE_DLLIB_CATEGORY_STR);
    size_t nbHandles = used.size();
    if (nbHandles > 0) {
        std::wstring fullLibraryName = libraryName;
        DynamicLinkLibraryObject* dlObject = nullptr;
        try {
            dlObject = new DynamicLinkLibraryObject(libraryName);
            fullLibraryName = dlObject->getLibraryPath();
            delete dlObject;
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        } catch (const Exception&) {
            retval << ArrayOf::logicalConstructor(false);
            if (nLhs > 1) {
                retval << ArrayOf::emptyConstructor();
            }
        }

        for (nelson_handle hl : used) {
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            auto* obj = (DynamicLinkLibraryObject*)hlObj;
            if (StringHelpers::ends_with(obj->getLibraryPath(), fullLibraryName)) {
                retval << ArrayOf::logicalConstructor(true);
                if (nLhs > 1) {
                    retval << ArrayOf::handleConstructor(hl);
                }
                return retval;
            }
        }
    } else {
        retval << ArrayOf::logicalConstructor(false);
        if (nLhs > 1) {
            retval << ArrayOf::emptyConstructor();
        }
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
