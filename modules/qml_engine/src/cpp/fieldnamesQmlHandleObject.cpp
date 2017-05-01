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
#pragma once
//=============================================================================
#include <QtQml/QQmlComponent>
#include "fieldnamesQmlHandleObject.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	void fieldnamesQmlHandleObject(ArrayOf A, bool fullList, wstringVector &fieldnames)
	{
		if (!A.isHandle())
		{
			throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
		}
		if (!A.isScalar())
		{
			throw Exception(ERROR_SIZE_SCALAR_EXPECTED);
		}
		nelson_handle *qp = (nelson_handle*)A.getDataPointer();
		if (qp == nullptr)
		{
			throw Exception(_W("QML valid handle expected."));
		}
		nelson_handle hl = qp[0];
		HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
		if (hlObj == nullptr)
		{
			throw Exception(_W("QML valid handle expected."));
		}
		if (hlObj->getCategory() != L"QML")
		{
			throw Exception(_W("QML handle expected."));
		}
		QmlHandleObject *qmlhandleobj = (QmlHandleObject *)hlObj;
		fieldnamesQmlHandleObject(qmlhandleobj, fullList, fieldnames);
	}
	//=============================================================================
	void fieldnamesQmlHandleObject(QmlHandleObject *qmlHandle, bool fullList, wstringVector &fieldnames)
	{
		void *ptr = qmlHandle->getPointer();
		fieldnames.clear();
		if (ptr == nullptr)
		{
			throw Exception(_W("QML valid handle expected."));
		}
		QObject *qobj = (QObject *)ptr;
		const QMetaObject *meta = qobj->metaObject();

		QMap<QString, QMetaProperty> list;
		for (int i = 0; i < meta->propertyCount(); i++)
		{
			QMetaProperty property = meta->property(i);
			const char* name = property.name();
			list[name] = property;
		}
		QMapIterator<QString, QMetaProperty> i(list);

		stringVector allFields;
		while (i.hasNext())
		{
			i.next();
			QMetaProperty property = i.value();
			allFields.push_back(property.name());
		}
		if (fullList)
		{
			for (size_t k = 0; k < allFields.size(); k++)
			{
				fieldnames.push_back(utf8_to_wstring(allFields[k]));
			}
		}
		else
		{
			QList<QObject *> widgets = qobj->findChildren<QObject *>(QString(), Qt::FindDirectChildrenOnly);
			for (size_t k = 0; k < allFields.size(); k++)
			{
				if (allFields[k] == "parent" || allFields[k] == "children")
				{
					fieldnames.push_back(utf8_to_wstring(allFields[k]));
				}
				else
				{
					QVariant propertyValue = qobj->property(allFields[k].c_str());
					if (propertyValue.isValid())
					{
						QVariant::Type qtype = propertyValue.type();
						switch (qtype)
						{
						case QMetaType::Bool:
						case QMetaType::Int:
						case QMetaType::UInt:
						case QMetaType::Double:
						case QMetaType::Float:
						case QMetaType::QString:
						case QMetaType::QUrl:
						case QMetaType::QColor:
						case QMetaType::QDate:
						case QMetaType::QPoint:
						case QMetaType::QPointF:
						case QMetaType::QSize:
						case QMetaType::QSizeF:
						case QMetaType::QRect:
						case QMetaType::QRectF:
						case QMetaType::QMatrix4x4:
						case QMetaType::QQuaternion:
						case QMetaType::QVector2D:
						case QMetaType::QVector3D:
						case QMetaType::QVector4D:
						case QMetaType::LongLong:
						case QMetaType::ULongLong:
						case QMetaType::QChar:
						case QMetaType::QStringList:
						case QMetaType::Long:
						case QMetaType::Short:
						case QMetaType::Char:
						case QMetaType::ULong:
						case QMetaType::UShort:
						case QMetaType::UChar:
						{
							fieldnames.push_back(utf8_to_wstring(allFields[k]));
						}
						break;
						default:
						{
							QObject * obj = qvariant_cast<QObject *>(propertyValue);
							if (obj != nullptr)
							{
								fieldnames.push_back(utf8_to_wstring(allFields[k]));
							}
						}
						break;
						}
					}
				}
			}
		}
	}
}
//=============================================================================
