//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "HandleGenericObject.hpp"
#include "nlsQml_engine_exports.h"
#include <string>
//=============================================================================
#define QOBJECT_CATEGORY_STR L"QObject"
#define QOBJECT_PROPERTY_PARENT_STR "parent"
#define QOBJECT_PROPERTY_CHILDREN_STR "children"
#define QOBJECT_PROPERTY_CLASSNAME_STR "className"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSQML_ENGINE_IMPEXP QmlHandleObject : public HandleGenericObject
{
public:
    QmlHandleObject(void* _ptr);
    ~QmlHandleObject();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
