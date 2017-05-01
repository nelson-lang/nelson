//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtQml/QQmlComponent>
#include "DeleteQmlHandleObject.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	bool DeleteQmlHandleObject(ArrayOf A)
	{
		bool res = false;
		if (A.isHandle())
		{
			if (!A.isEmpty())
			{
				if (A.isScalar())
				{
					nelson_handle *qp = (nelson_handle*)A.getDataPointer();
					nelson_handle hl = qp[0];
					HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
					if (hlObj)
					{
						if (hlObj->getCategory() != L"QML")
						{
							throw Exception(_W("QML handle expected."));
						}

						QmlHandleObject *qmlhandleobj = (QmlHandleObject *)hlObj;
						void *ptr = qmlhandleobj->getPointer();
						if (ptr)
						{
							bool deletedByTheParent = false;

							QObject *qobj = (QObject *)ptr;
							QObject *qobjParent = qobj->parent();
							if (qobjParent != nullptr)
							{
								bool isWindow = qobjParent->isWindowType();
								if (isWindow)
								{
									delete qobjParent;
									deletedByTheParent = true;
								}
							}
							if (!deletedByTheParent)
							{
								qobj->deleteLater();
								delete qobj;
							}

							qmlhandleobj->setPointer(nullptr);
						}
						delete qmlhandleobj;
					}
					HandleManager::getInstance()->removeHandle(hl);
					res = true;
				}
				else
				{
					throw Exception(_W("QML scalar handle expected."));
				}
			}
		}
		return res;
	}
	//=============================================================================
}
//=============================================================================
