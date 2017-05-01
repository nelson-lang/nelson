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
#include "GetQmlHandleObject.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "HandleGenericObject.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "ToCellString.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf GetQmlHandleObject(ArrayOf A, std::wstring propertyName)
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
            QObject *qparent = qobj->parent();
            if (qparent)
            {
                QmlHandleObject * qmlHandle = nullptr;
                try
                {
                    qmlHandle = new QmlHandleObject(qparent);
                }
                catch (std::bad_alloc &e)
                {
                    e.what();
                    qmlHandle = nullptr;
                    throw Exception(ERROR_MEMORY_ALLOCATION);
                }
                res = ArrayOf::handleConstructor(qmlHandle);
            }
            else
            {
                throw Exception(_W("No parent."));
            }
        }
        else if (propertyName == L"children")
        {
            QObjectList childs = qobj->children();
            int nbChilds = childs.size();
            if (nbChilds == 0)
            {
                Dimensions dims(0, 0);
                res = ArrayOf::emptyConstructor(dims);
                res.promoteType(NLS_HANDLE);
            }
            Dimensions dims(1, nbChilds);
            nelson_handle *nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, nbChilds);
            for (int k = 0; k < nbChilds; k++)
            {
                QmlHandleObject * qmlHandle = nullptr;
                try
                {
                    qmlHandle = new QmlHandleObject(childs[k]);
                }
                catch (std::bad_alloc &e)
                {
                    e.what();
                    qmlHandle = nullptr;
                    throw Exception(ERROR_MEMORY_ALLOCATION);
                }
                nh[k] = HandleManager::getInstance()->addHandle(qmlHandle);
            }
            res =ArrayOf(NLS_HANDLE, dims, (void *)nh);
        }
        else
        {
            QVariant propertyValue = qobj->property(wstring_to_utf8(propertyName).c_str());
            if (propertyValue.isValid())
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
                        res = ArrayOf::logicalConstructor(propertyValue.toBool());
                    }
                    break;
                    case QMetaType::Int:
                    {
                        res = ArrayOf::int32Constructor(propertyValue.toInt());
                    }
                    break;
                    case QMetaType::UInt:
                    {
                        res = ArrayOf::uint32Constructor(propertyValue.toUInt());
                    }
                    break;
                    case QMetaType::Double:
                    {
                        res = ArrayOf::doubleConstructor(propertyValue.toDouble());
                    }
                    break;
                    case QMetaType::Float:
                    {
                        res = ArrayOf::singleConstructor(propertyValue.toFloat());
                    }
                    break;
                    case QMetaType::QString:
                    {
                        res = ArrayOf::stringConstructor(QStringTowstring(propertyValue.toString()));
                    }
                    break;
                    case QMetaType::QUrl:
                    {
                        QUrl qurl = qvariant_cast<QUrl>(propertyValue);
                        res = ArrayOf::stringConstructor(QStringTowstring(qurl.toString()));
                    }
                    break;
                    case QMetaType::QColor:
                    {
                        QColor qcolor = qvariant_cast<QColor>(propertyValue);
                        int32 *arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 4);
                        arrayInt32[0] = qcolor.red();
                        arrayInt32[1] = qcolor.green();
                        arrayInt32[2] = qcolor.blue();
                        arrayInt32[3] = qcolor.alpha();
                        Dimensions dims(1, 4);
                        res = ArrayOf::ArrayOf(NLS_INT32, dims, arrayInt32);
                    }
                    break;
                    case QMetaType::QDate:
                    {
                        QDate qdate = qvariant_cast<QDate>(propertyValue);
                        int32 *arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 3);
                        arrayInt32[0] = qdate.year();
                        arrayInt32[1] = qdate.month();
                        arrayInt32[2] = qdate.day();
                        Dimensions dims(1, 3);
                        res = ArrayOf::ArrayOf(NLS_INT32, dims, arrayInt32);
                    }
                    break;
                    case QMetaType::QPoint:
                    {
                        QPoint qpoint = propertyValue.toPoint();
                        int32 *arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 2);
                        arrayInt32[0] = qpoint.x();
                        arrayInt32[1] = qpoint.y();
                        Dimensions dims(1, 2);
                        res = ArrayOf::ArrayOf(NLS_INT32, dims, arrayInt32);
                    }
                    break;
                    case QMetaType::QPointF:
                    {
                        QPointF qpointf = propertyValue.toPointF();
                        double *arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 2);
                        arrayDouble[0] = qpointf.x();
                        arrayDouble[1] = qpointf.y();
                        Dimensions dims(1, 2);
                        res = ArrayOf::ArrayOf(NLS_DOUBLE, dims, arrayDouble);
                    }
                    break;
                    case QMetaType::QSize:
                    {
                        QSize qsize = propertyValue.toSize();
                        int32 *arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 2);
                        arrayInt32[0] = qsize.width();
                        arrayInt32[1] = qsize.height();
                        Dimensions dims(1, 2);
                        res = ArrayOf::ArrayOf(NLS_INT32, dims, arrayInt32);
                    }
                    break;
                    case QMetaType::QSizeF:
                    {
                        QSizeF qsizef = propertyValue.toSizeF();
                        double *arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_INT32, 2);
                        arrayDouble[0] = qsizef.width();
                        arrayDouble[1] = qsizef.height();
                        Dimensions dims(1, 2);
                        res = ArrayOf::ArrayOf(NLS_INT32, dims, arrayDouble);
                    }
                    break;
                    case QMetaType::QRect:
                    {
                        QRect qrect = propertyValue.toRect();
                        int32 *arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 4);
                        arrayInt32[0] = qrect.x();
                        arrayInt32[1] = qrect.y();
                        arrayInt32[2] = qrect.width();
                        arrayInt32[3] = qrect.height();
                        Dimensions dims(1, 4);
                        res = ArrayOf::ArrayOf(NLS_INT32, dims, arrayInt32);
                    }
                    break;
                    case QMetaType::QRectF:
                    {
                        QRectF qrectf = propertyValue.toRectF();
                        double *arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 4);
                        arrayDouble[0] = (double)qrectf.x();
                        arrayDouble[1] = (double)qrectf.y();
                        arrayDouble[2] = (double)qrectf.width();
                        arrayDouble[3] = (double)qrectf.height();
                        Dimensions dims(1, 4);
                        res = ArrayOf::ArrayOf(NLS_DOUBLE, dims, arrayDouble);
                    }
                    break;
                    case QMetaType::QMatrix4x4:
                    {
                        QMatrix4x4 qmatrix4x4 = qvariant_cast<QMatrix4x4>(propertyValue);
                        single *arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 4 * 4);
                        single *ptr = qmatrix4x4.data();
                        Dimensions dims(4, 4);
                        for (size_t k = 0; k < dims.getElementCount(); k++)
                        {
                            arraySingle[k] = ptr[k];
                        }
                        res = ArrayOf::ArrayOf(NLS_DOUBLE, dims, arraySingle);
                    }
                    break;
                    case QMetaType::QQuaternion:
                    {
                        QQuaternion qq = qvariant_cast<QQuaternion>(propertyValue);
                        single *arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 4);
                        arraySingle[0] = qq.scalar();
                        arraySingle[1] = qq.x();
                        arraySingle[2] = qq.y();
                        arraySingle[3] = qq.z();
                        Dimensions dims(1, 4);
                        res = ArrayOf::ArrayOf(NLS_SINGLE, dims, arraySingle);
                    }
                    break;
                    case QMetaType::QVector2D:
                    {
                        QVector2D qvector2d = qvariant_cast<QVector2D>(propertyValue);
                        single *arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 2);
                        arraySingle[0] = qvector2d.x();
                        arraySingle[1] = qvector2d.y();
                        Dimensions dims(1, 2);
                        res = ArrayOf::ArrayOf(NLS_SINGLE, dims, arraySingle);
                    }
                    break;
                    case QMetaType::QVector3D:
                    {
                        QVector3D qvector3d = qvariant_cast<QVector3D>(propertyValue);
                        single *arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 3);
                        arraySingle[0] = qvector3d.x();
                        arraySingle[1] = qvector3d.y();
                        arraySingle[2] = qvector3d.z();
                        Dimensions dims(1, 3);
                        res = ArrayOf::ArrayOf(NLS_SINGLE, dims, arraySingle);
                    }
                    break;
                    case QMetaType::QVector4D:
                    {
                        QVector4D qvector4d = qvariant_cast<QVector4D>(propertyValue);
                        single *arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 4);
                        arraySingle[0] = qvector4d.x();
                        arraySingle[1] = qvector4d.y();
                        arraySingle[2] = qvector4d.z();
                        arraySingle[3] = qvector4d.w();
                        Dimensions dims(1,4);
                        res = ArrayOf::ArrayOf(NLS_SINGLE, dims, arraySingle);
                    }
                    break;
                    case QMetaType::LongLong:
                    {
                        res = ArrayOf::int64Constructor(propertyValue.toLongLong());
                    }
                    break;
                    case QMetaType::ULongLong:
                    {
                        res = ArrayOf::uint64Constructor(propertyValue.toULongLong());
                    }
                    break;
                    case QMetaType::QChar:
                    {
                        QChar qchar = qvariant_cast<QChar>(propertyValue);
                        QString qstring(qchar);
                        res = ArrayOf::stringConstructor(QStringTowstring(qstring));
                    }
                    break;
                    case QMetaType::QStringList:
                    {
                        QStringList stringlist = qvariant_cast<QStringList>(propertyValue);
                        wstringVector wvector;
                        for (int k = 0; k < stringlist.size(); k++)
                        {
                            wvector.push_back(QStringTowstring(stringlist[k]));
                        }
                        res = ToCellStringAsRow(wvector);
                    }
                    break;
                    case QMetaType::Long:
                    {
                        long l = qvariant_cast<long>(propertyValue);
                        int64 i64 = (int64)l;
                        res = ArrayOf::int64Constructor(i64);
                    }
                    break;
                    case QMetaType::Short:
                    {
                        int16 s = qvariant_cast<int16>(propertyValue);
                        res = ArrayOf::int16Constructor(s);
                    }
                    break;
                    case QMetaType::Char:
                    {
                        char c = qvariant_cast<char>(propertyValue);
                        int8 i8 = (int8)c;
                        res = ArrayOf::int8Constructor(i8);
                    }
                    break;
                    case QMetaType::ULong:
                    {
                        unsigned long ul = qvariant_cast<unsigned long>(propertyValue);
                        uint64 ui64 = (uint64)ul;
                        res = ArrayOf::uint64Constructor(ui64);
                    }
                    break;
                    case QMetaType::UShort:
                    {
                        uint16 ul = qvariant_cast<uint16>(propertyValue);
                        uint16 ui16 = (uint16)ul;
                        res = ArrayOf::uint16Constructor(ui16);
                    }
                    break;
                    case QMetaType::UChar:
                    {
                        unsigned char uc = qvariant_cast<unsigned char>(propertyValue);
                        uint8 ui8 = (uint8)uc;
                        res = ArrayOf::uint8Constructor(ui8);
                    }
                    break;
                    default:
                    {
                        QObject * obj = qvariant_cast<QObject *>(propertyValue);
                        if (obj == nullptr)
                        {
                            throw Exception(_W("property type not managed."));
                        }
                        QmlHandleObject * qmlHandle = nullptr;
                        try
                        {
                            QObject * obj = qvariant_cast<QObject *>(propertyValue);
                            qmlHandle = new QmlHandleObject(obj);
                        }
                        catch (std::bad_alloc &e)
                        {
                            e.what();
                            qmlHandle = nullptr;
                            throw Exception(ERROR_MEMORY_ALLOCATION);
                        }
                        res = ArrayOf::handleConstructor(qmlHandle);
                    }
                    break;
                }
            }
        }
        return res;
    }
    //=============================================================================
}
//=============================================================================
