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
#include <QtGui/QQuaternion>
#include <QtGui/qcolor.h>
#include <QtGui/QVector2D>
#include <QtGui/qmatrix4x4.h>
#include <QtCore/qrect.h>
#include <QtCore/qdatetime.h>
#include <QtCore/QStringList>
#include "SetQmlHandleObject.hpp"
#include "Exception.hpp"
#include "HandleGenericObject.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void SetQmlHandleObject(ArrayOf A, std::wstring propertyName, ArrayOf B)
    {
        ArrayOf res;
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
        void *ptr = qmlhandleobj->getPointer();
        if (ptr == nullptr)
        {
            throw Exception(_W("QML valid handle expected."));
        }
        QObject *qobj = (QObject *)ptr;
        if (propertyName == L"parent")
        {
            throw Exception(_W("'parent' can not modified."));
        }
        else if (propertyName == L"children")
        {
            throw Exception(_W("'children' can not modified."));
        }
        else
        {
            QVariant propertyValue = qobj->property(wstring_to_utf8(propertyName).c_str());
            if (!propertyValue.isValid())
            {
            }
            else
            {
                QVariant::Type qtype = propertyValue.type();
                switch (qtype)
                {
                    case QMetaType::UnknownType:
                    {
                        throw Exception(_W("property unknown type."));
                    }
                    break;
                    case QMetaType::Bool:
                    {
                        bool res;
                        if (B.getContentAsLogicalScalar() == 1)
                        {
                            res = true;
                        }
                        else
                        {
                            res = false;
                        }
                        qobj->setProperty(wstring_to_utf8(propertyName).c_str(), res);
                    }
                    break;
                    case QMetaType::Int:
                    {
                    }
                    break;
                    case QMetaType::UInt:
                    {
                    }
                    break;
                    case QMetaType::Double:
                    {
                    }
                    break;
                    case QMetaType::Float:
                    {
                    }
                    break;
                    case QMetaType::QString:
                    {
                        std::wstring wstr = B.getContentsAsWideString();
                        qobj->setProperty(wstring_to_utf8(propertyName).c_str(), wstringToQString(wstr));
                    }
                    break;
                    case QMetaType::QUrl:
                    {
                    }
                    break;
                    case QMetaType::QColor:
                    {
                    }
                    break;
                    case QMetaType::QDate:
                    {
                    }
                    break;
                    case QMetaType::QPoint:
                    {
                    }
                    break;
                    case QMetaType::QPointF:
                    {
                    }
                    break;
                    case QMetaType::QSize:
                    {
                    }
                    break;
                    case QMetaType::QSizeF:
                    {
                    }
                    break;
                    case QMetaType::QRect:
                    {
                    }
                    break;
                    case QMetaType::QRectF:
                    {
                    }
                    break;
                    case QMetaType::QMatrix4x4:
                    {
                    }
                    break;
                    case QMetaType::QQuaternion:
                    {
                    }
                    break;
                    case QMetaType::QVector2D:
                    {
                    }
                    break;
                    case QMetaType::QVector3D:
                    {
                    }
                    break;
                    case QMetaType::QVector4D:
                    {
                    }
                    break;
                    case QMetaType::LongLong:
                    {
                    }
                    break;
                    case QMetaType::ULongLong:
                    {
                    }
                    break;
                    case QMetaType::QChar:
                    {
                    }
                    break;
                    case QMetaType::QStringList:
                    {
                    }
                    break;
                    case QMetaType::Long:
                    {
                    }
                    break;
                    case QMetaType::Short:
                    {
                    }
                    break;
                    case QMetaType::Char:
                    {
                    }
                    break;
                    case QMetaType::ULong:
                    {
                    }
                    break;
                    case QMetaType::UShort:
                    {
                    }
                    break;
                    case QMetaType::UChar:
                    {
                    }
                    break;
                    default:
                    {
                        throw Exception(_W("property type not managed."));
                    }
                    break;
                }
            }
        }
    }
    //=============================================================================
}
//=============================================================================
