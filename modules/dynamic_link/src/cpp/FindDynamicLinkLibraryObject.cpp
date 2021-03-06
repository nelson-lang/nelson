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
#include <boost/algorithm/string.hpp>
#include "FindDynamicLinkLibraryObject.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include "StringFormat.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
findDynamicLinkLibraryObject(const std::wstring& libraryName, int nLhs)
{
    ArrayOfVector retval;

    std::vector<nelson_handle> used
        = HandleManager::getInstance()->getAllHandlesOfCategory(DLLIB_CATEGORY_STR);
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
            if (boost::algorithm::ends_with(obj->getLibraryPath(), fullLibraryName)) {
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
