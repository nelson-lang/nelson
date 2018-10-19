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
#include "QVariantArrayOf.hpp"
#include "Error.hpp"
#include "GetQmlHandleObject.hpp"
#include "HandleManager.hpp"
#include "IsCellOfStrings.hpp"
#include "QStringConverter.hpp"
#include "QmlHandleObject.hpp"
#include "ToCellString.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QBitArray>
#include <QtCore/QDateTime>
#include <QtCore/QLine>
#include <QtCore/QLineF>
#include <QtCore/QMetaType>
#include <QtCore/QRect>
#include <QtCore/QStringList>
#include <QtCore/QUrl>
#include <QtCore/QUuid>
#include <QtGui/QColor>
#include <QtGui/QMatrix4x4>
#include <QtGui/QMatrix>
#include <QtGui/QQuaternion>
#include <QtGui/QTransform>
#include <QtGui/QVector2D>
#include <QtQml/QJSValue>
#include <QtQml/QQmlListProperty>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
canBeConvertedToArrayOf(QVariant Q)
{
    bool res = false;
    QVariant::Type typeDest = Q.type();
    switch (typeDest) {
    case QVariant::Type::Bool:
    case QVariant::Type::Int:
    case QVariant::Type::UInt:
    case QVariant::Type::LongLong:
    case QVariant::Type::ULongLong:
    case QVariant::Type::Double:
    case QVariant::Type::Char:
    case QVariant::Type::String:
    case QVariant::Type::StringList:
    case QVariant::Type::ByteArray:
    case QVariant::Type::BitArray:
    case QVariant::Type::Date:
    case QVariant::Type::Time:
    case QVariant::Type::DateTime:
    case QVariant::Type::Url:
    case QVariant::Type::Rect:
    case QVariant::Type::RectF:
    case QVariant::Type::Size:
    case QVariant::Type::SizeF:
    case QVariant::Type::Line:
    case QVariant::Type::LineF:
    case QVariant::Type::Point:
    case QVariant::Type::PointF:
    case QVariant::Type::Uuid:
    case QVariant::Type::Color:
    case QVariant::Type::Matrix:
    case QVariant::Type::Transform:
    case QVariant::Type::Matrix4x4:
    case QVariant::Type::Vector2D:
    case QVariant::Type::Vector3D:
    case QVariant::Type::Vector4D:
    case QVariant::Type::Quaternion: {
        res = true;
    } break;
    case QVariant::Type::List: {
        QList<QVariant> qlistVariant = qvariant_cast<QList<QVariant>>(Q);
        for (int k = 0; k < qlistVariant.size(); k++) {
            if (!canBeConvertedToArrayOf(qlistVariant[k])) {
                return false;
            }
        }
        return true;
    } break;
    case QVariant::Type::Map: {
        QVariantMap qvariantMap = qvariant_cast<QVariantMap>(Q);
        for (QVariantMap::const_iterator iter = qvariantMap.begin(); iter != qvariantMap.end();
             ++iter) {
            if (canBeConvertedToArrayOf(iter.value()) == false) {
                return false;
            }
        }
        return true;
    } break;
    case QVariant::Type::Palette:
    case QVariant::Type::Image:
    case QVariant::Type::KeySequence:
    case QVariant::Type::SizePolicy:
    case QVariant::Type::Locale:
    case QVariant::Type::TextLength:
    case QVariant::Type::TextFormat:
    case QVariant::Type::Pen:
    case QVariant::Type::UserType:
    case QVariant::Type::LastType:
    case QVariant::Type::Invalid:
    case QVariant::Type::RegExp:
    case QVariant::Type::RegularExpression:
    case QVariant::Type::Hash:
    case QVariant::Type::EasingCurve:
    case QVariant::Type::ModelIndex:
    case QVariant::Type::Font:
    case QVariant::Type::Pixmap:
    case QVariant::Type::Brush:
    case QVariant::Type::Icon:
    case QVariant::Type::Bitmap:
    case QVariant::Type::Cursor:
    case QVariant::Type::Polygon:
    case QVariant::Type::PolygonF:
    case QVariant::Type::Region:
    default: {
        if (Q.type() == (int)QMetaType::QObjectStar || Q.canConvert<QObject*>()) {
            return true;
        }
        QQmlListReference ref = Q.value<QQmlListReference>();
        if (ref.isValid() && ref.canCount() && ref.canAt()) {
            int len = ref.count();
            if (len > 0) {
                return true;
            }
        }
        const char* name = Q.typeName();
        if (strncmp(name, "QQmlListProperty<", 17) == 0) {
            QQmlListProperty<QObject>* list
                = reinterpret_cast<QQmlListProperty<QObject>*>(Q.data());
            int nbChilds = 0;
            if (list->count && list->at) {
                nbChilds = list->count(list);
            }
            if (nbChilds > 0) {
                res = true;
            } else {
                res = false;
            }
        } else if (strcmp(name, "QJSValue") == 0) {
            QVariant v = Q.value<QJSValue>().toVariant();
            res = v.isValid();
        } else {
            res = false;
        }
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
QVariantToArrayOf(QVariant Q)
{
    ArrayOf res;
    if (!Q.isValid()) {
        Error(_W("QVariant invalid."));
    }
    QVariant::Type qtype = Q.type();
    switch (qtype) {
    case QVariant::Type::Bool: {
        return ArrayOf::logicalConstructor(Q.toBool());
    } break;
    case QVariant::Type::Int: {
        return ArrayOf::int32Constructor(Q.toInt());
    } break;
    case QVariant::Type::UInt: {
        return ArrayOf::uint32Constructor(Q.toUInt());
    } break;
    case QVariant::Type::LongLong: {
        return ArrayOf::int64Constructor(Q.toLongLong());
    } break;
    case QVariant::Type::ULongLong: {
        return ArrayOf::uint64Constructor(Q.toULongLong());
    } break;
    case QVariant::Type::Double: {
        return ArrayOf::doubleConstructor(Q.toDouble());
    } break;
    case QVariant::Type::Char: {
        char c = qvariant_cast<char>(Q);
        int8 i8 = (int8)c;
        return ArrayOf::int8Constructor(i8);
    } break;
    case QVariant::Type::String: {
        return ArrayOf::characterArrayConstructor(QStringTowstring(Q.toString()));
    } break;
    case QVariant::Type::StringList: {
        QStringList stringlist = qvariant_cast<QStringList>(Q);
        wstringVector wvector;
        for (int k = 0; k < stringlist.size(); k++) {
            wvector.push_back(QStringTowstring(stringlist[k]));
        }
        return ToCellStringAsRow(wvector);
    } break;
    case QVariant::Type::ByteArray: {
        QByteArray qbytearray = Q.toByteArray();
        int count = qbytearray.count();
        const char* data = qbytearray.data();
        int8* arrayInt8 = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, count);
        for (int k = 0; k < count; k++) {
            arrayInt8[k] = (int8)data[k];
        }
        Dimensions dims(1, count);
        return ArrayOf(NLS_INT8, dims, (void*)arrayInt8);
    } break;
    case QVariant::Type::BitArray: {
        QBitArray qbitarray = Q.toBitArray();
        int count = qbitarray.count();
        logical* arrayLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, count);
        for (int k = 0; k < count; k++) {
            arrayLogical[k] = (logical)qbitarray[k];
        }
        Dimensions dims(1, count);
        return ArrayOf(NLS_LOGICAL, dims, (void*)arrayLogical);
    } break;
    case QVariant::Type::Date: {
        QDate qdate = qvariant_cast<QDate>(Q);
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 3);
        arrayInt32[0] = qdate.year();
        arrayInt32[1] = qdate.month();
        arrayInt32[2] = qdate.day();
        Dimensions dims(1, 3);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::Time: {
        QTime qtime = qvariant_cast<QTime>(Q);
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 4);
        arrayInt32[0] = qtime.hour();
        arrayInt32[1] = qtime.minute();
        arrayInt32[2] = qtime.second();
        arrayInt32[3] = qtime.msec();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::DateTime: {
        QDateTime qdatetime = qvariant_cast<QDateTime>(Q);
        qdatetime.setTimeSpec(Qt::UTC); // FORCE UTC
        QDate qdate = qdatetime.date();
        QTime qtime = qdatetime.time();
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 7);
        arrayInt32[0] = qdate.year();
        arrayInt32[1] = qdate.month();
        arrayInt32[2] = qdate.day();
        arrayInt32[3] = qtime.hour();
        arrayInt32[4] = qtime.minute();
        arrayInt32[5] = qtime.second();
        arrayInt32[6] = qtime.msec();
        Dimensions dims(1, 7);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::Url: {
        QUrl qurl = qvariant_cast<QUrl>(Q);
        res = ArrayOf::characterArrayConstructor(QStringTowstring(qurl.toString()));
    } break;
    case QVariant::Type::Rect: {
        QRect qrect = Q.toRect();
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 4);
        arrayInt32[0] = qrect.x();
        arrayInt32[1] = qrect.y();
        arrayInt32[2] = qrect.width();
        arrayInt32[3] = qrect.height();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::RectF: {
        QRectF qrectf = Q.toRectF();
        double* arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 4);
        arrayDouble[0] = (double)qrectf.x();
        arrayDouble[1] = (double)qrectf.y();
        arrayDouble[2] = (double)qrectf.width();
        arrayDouble[3] = (double)qrectf.height();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_DOUBLE, dims, (void*)arrayDouble);
    } break;
    case QVariant::Type::Size: {
        QSize qsize = Q.toSize();
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 2);
        arrayInt32[0] = qsize.width();
        arrayInt32[1] = qsize.height();
        Dimensions dims(1, 2);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::SizeF: {
        QSizeF qsizef = Q.toSizeF();
        double* arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_INT32, 2);
        arrayDouble[0] = qsizef.width();
        arrayDouble[1] = qsizef.height();
        Dimensions dims(1, 2);
        return ArrayOf(NLS_INT32, dims, (void*)arrayDouble);
    } break;
    case QVariant::Type::Line: {
        QLine qline = Q.toLine();
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 4);
        arrayInt32[0] = qline.x1();
        arrayInt32[1] = qline.y1();
        arrayInt32[2] = qline.x2();
        arrayInt32[3] = qline.y2();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::LineF: {
        QLineF qlinef = Q.toLineF();
        double* arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 4);
        arrayDouble[0] = (double)qlinef.x1();
        arrayDouble[1] = (double)qlinef.y1();
        arrayDouble[2] = (double)qlinef.x2();
        arrayDouble[3] = (double)qlinef.y2();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_DOUBLE, dims, (void*)arrayDouble);
    } break;
    case QVariant::Type::Point: {
        QPoint qpoint = Q.toPoint();
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 2);
        arrayInt32[0] = qpoint.x();
        arrayInt32[1] = qpoint.y();
        Dimensions dims(1, 2);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::PointF: {
        QPointF qpointf = Q.toPointF();
        double* arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 2);
        arrayDouble[0] = (double)qpointf.x();
        arrayDouble[1] = (double)qpointf.y();
        Dimensions dims(1, 2);
        return ArrayOf(NLS_DOUBLE, dims, (void*)arrayDouble);
    } break;
    case QVariant::Type::Uuid: {
        QUuid quuid = Q.toUuid();
        return ArrayOf::characterArrayConstructor(QStringTowstring(quuid.toString()));
    } break;
    case QVariant::Type::Color: {
        QColor qcolor = qvariant_cast<QColor>(Q);
        int32* arrayInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, 4);
        arrayInt32[0] = qcolor.red();
        arrayInt32[1] = qcolor.green();
        arrayInt32[2] = qcolor.blue();
        arrayInt32[3] = qcolor.alpha();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_INT32, dims, (void*)arrayInt32);
    } break;
    case QVariant::Type::Matrix: {
        QMatrix qmatrix = qvariant_cast<QMatrix>(Q);
        double* arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 6);
        arrayDouble[0] = (double)qmatrix.m11();
        arrayDouble[1] = (double)qmatrix.m12();
        arrayDouble[2] = (double)qmatrix.m21();
        arrayDouble[3] = (double)qmatrix.m22();
        arrayDouble[4] = (double)qmatrix.dx();
        arrayDouble[5] = (double)qmatrix.dy();
        Dimensions dims(1, 6);
        return ArrayOf(NLS_DOUBLE, dims, (void*)arrayDouble);
    } break;
    case QVariant::Type::Transform: {
        QTransform qtransform = qvariant_cast<QTransform>(Q);
        double* arrayDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 9);
        arrayDouble[0] = (double)qtransform.m11();
        arrayDouble[1] = (double)qtransform.m12();
        arrayDouble[2] = (double)qtransform.m13();
        arrayDouble[3] = (double)qtransform.m21();
        arrayDouble[4] = (double)qtransform.m22();
        arrayDouble[5] = (double)qtransform.m23();
        arrayDouble[6] = (double)qtransform.m31();
        arrayDouble[7] = (double)qtransform.m32();
        arrayDouble[8] = (double)qtransform.m33();
        Dimensions dims(3, 3);
        return ArrayOf(NLS_DOUBLE, dims, (void*)arrayDouble);
    } break;
    case QVariant::Type::Matrix4x4: {
        QMatrix4x4 qmatrix4x4 = qvariant_cast<QMatrix4x4>(Q);
        single* arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 16);
        const single* data = qmatrix4x4.data();
        for (int k = 0; k < 16; k++) {
            arraySingle[k] = (single)data[k];
        }
        Dimensions dims(4, 4);
        return ArrayOf(NLS_SINGLE, dims, (void*)arraySingle);
    } break;
    case QVariant::Type::Vector2D: {
        QVector2D qvector2d = qvariant_cast<QVector2D>(Q);
        single* arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 2);
        arraySingle[0] = qvector2d.x();
        arraySingle[1] = qvector2d.y();
        Dimensions dims(1, 2);
        return ArrayOf(NLS_SINGLE, dims, (void*)arraySingle);
    } break;
    case QVariant::Type::Vector3D: {
        QVector3D qvector3d = qvariant_cast<QVector3D>(Q);
        single* arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 3);
        arraySingle[0] = qvector3d.x();
        arraySingle[1] = qvector3d.y();
        arraySingle[2] = qvector3d.z();
        Dimensions dims(1, 3);
        return ArrayOf(NLS_SINGLE, dims, (void*)arraySingle);
    } break;
    case QVariant::Type::Vector4D: {
        QVector4D qvector4d = qvariant_cast<QVector4D>(Q);
        single* arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 4);
        arraySingle[0] = qvector4d.x();
        arraySingle[1] = qvector4d.y();
        arraySingle[2] = qvector4d.z();
        arraySingle[3] = qvector4d.w();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_SINGLE, dims, (void*)arraySingle);
    } break;
    case QVariant::Type::Quaternion: {
        QQuaternion qq = qvariant_cast<QQuaternion>(Q);
        single* arraySingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, 4);
        arraySingle[0] = qq.scalar();
        arraySingle[1] = qq.x();
        arraySingle[2] = qq.y();
        arraySingle[3] = qq.z();
        Dimensions dims(1, 4);
        return ArrayOf(NLS_SINGLE, dims, (void*)arraySingle);
    } break;
    case QVariant::Type::List: {
        QVariantList qlistVariant = qvariant_cast<QVariantList>(Q);
        Dimensions dimsCellArray(1, qlistVariant.size());
        ArrayOf* cellArray
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dimsCellArray.getElementCount());
        for (int k = 0; k < qlistVariant.size(); k++) {
            cellArray[k] = QVariantToArrayOf(qlistVariant[k]);
        }
        return ArrayOf(NLS_CELL_ARRAY, dimsCellArray, cellArray);
    } break;
    case QVariant::Type::Map: {
        QVariantMap qvariantMap = qvariant_cast<QVariantMap>(Q);
        wstringVector fieldnames;
        ArrayOfVector fieldvalues;
        for (QVariantMap::const_iterator iter = qvariantMap.begin(); iter != qvariantMap.end();
             ++iter) {
            fieldnames.push_back(QStringTowstring(iter.key()));
            fieldvalues.push_back(QVariantToArrayOf(iter.value()));
        }
        ArrayOf structArray = ArrayOf::emptyConstructor(0, 0);
        for (size_t k = 0; k < fieldnames.size(); k++) {
            structArray.setField(wstring_to_utf8(fieldnames[k]), fieldvalues[k]);
        }
        return structArray;
    } break;
    default: {
        if (Q.type() == (int)QMetaType::QObjectStar || Q.canConvert<QObject*>()) {
            QObject* qobject = Q.value<QObject*>();
            if (qobject) {
                Dimensions dims(1, 1);
                nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, 1);
                nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qobject);
                if (nh_found != -1) {
                    nh[0] = nh_found;
                } else {
                    QmlHandleObject* qmlHandle = nullptr;
                    try {
                        qmlHandle = new QmlHandleObject(qobject);
                    } catch (const std::bad_alloc& e) {
                        e.what();
                        qmlHandle = nullptr;
                        Error(ERROR_MEMORY_ALLOCATION);
                    }
                    nh[0] = HandleManager::getInstance()->addHandle(qmlHandle);
                }
                return ArrayOf(NLS_HANDLE, dims, (void*)nh);
            }
        }
        QQmlListReference ref = Q.value<QQmlListReference>();
        if (ref.isValid() && ref.canCount() && ref.canAt()) {
            int nbChilds = ref.count();
            if (nbChilds == 0) {
                Dimensions dims(0, 0);
                res = ArrayOf::emptyConstructor(dims);
                res.promoteType(NLS_HANDLE);
                return res;
            } else {
                Dimensions dims(1, nbChilds);
                nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, nbChilds);
                for (int k = 0; k < nbChilds; k++) {
                    QObject* qobj = ref.at(k);
                    nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qobj);
                    if (nh_found != -1) {
                        nh[k] = nh_found;
                    } else {
                        QmlHandleObject* qmlHandle = nullptr;
                        try {
                            qmlHandle = new QmlHandleObject(qobj);
                        } catch (const std::bad_alloc& e) {
                            e.what();
                            qmlHandle = nullptr;
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        nh[k] = HandleManager::getInstance()->addHandle(qmlHandle);
                    }
                }
                return ArrayOf(NLS_HANDLE, dims, (void*)nh);
            }
        }
        const char* name = Q.typeName();
        if (strncmp(name, "QQmlListProperty<", 17) == 0) {
            QQmlListProperty<QObject>* list
                = reinterpret_cast<QQmlListProperty<QObject>*>(Q.data());
            int nbChilds = 0;
            if (list->count && list->at) {
                nbChilds = list->count(list);
            }
            if (nbChilds == 0) {
                Dimensions dims(0, 0);
                res = ArrayOf::emptyConstructor(dims);
                res.promoteType(NLS_HANDLE);
                return res;
            } else {
                Dimensions dims(1, nbChilds);
                nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, nbChilds);
                for (int k = 0; k < nbChilds; k++) {
                    QObject* qobj = list->at(list, k);
                    nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qobj);
                    if (nh_found != -1) {
                        nh[k] = nh_found;
                    } else {
                        QmlHandleObject* qmlHandle = nullptr;
                        try {
                            qmlHandle = new QmlHandleObject(qobj);
                        } catch (const std::bad_alloc& e) {
                            e.what();
                            qmlHandle = nullptr;
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        nh[k] = HandleManager::getInstance()->addHandle(qmlHandle);
                    }
                }
                return ArrayOf(NLS_HANDLE, dims, (void*)nh);
            }
        } else if (strcmp(name, "QJSValue") == 0) {
            Q = Q.value<QJSValue>().toVariant();
            return QVariantToArrayOf(Q);
        }
        QObject* obj = qvariant_cast<QObject*>(Q);
        if (obj == nullptr) {
            Error(_W("property type not managed."));
        }
        QmlHandleObject* qmlHandle = nullptr;
        try {
            QObject* obj = qvariant_cast<QObject*>(Q);
            qmlHandle = new QmlHandleObject(obj);
        } catch (const std::bad_alloc& e) {
            e.what();
            qmlHandle = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        return ArrayOf::handleConstructor(qmlHandle);
    } break;
    }
    return res;
}
//=============================================================================
QVariant
ArrayOfToQVariant(ArrayOf A, QVariant::Type typeDest)
{
    QVariant res;
    switch (typeDest) {
    case QVariant::Type::Bool: {
        if (A.getContentAsLogicalScalar() == 1) {
            res = true;
        } else {
            res = false;
        }
    } break;
    case QVariant::Type::Int: {
        int32 v = A.getContentAsInteger32Scalar();
        res = v;
    } break;
    case QVariant::Type::UInt: {
        uint32 v = A.getContentAsUnsignedInteger32Scalar();
        res = v;
    } break;
    case QVariant::Type::LongLong: {
        int64 v = A.getContentAsInteger64Scalar();
        res = QVariant((long long)v);
    } break;
    case QVariant::Type::ULongLong: {
        uint64 v = A.getContentAsUnsignedInt64Scalar();
        res = QVariant((unsigned long long)v);
    } break;
    case QVariant::Type::Double: {
        double v = A.getContentAsDoubleScalar();
        res = v;
    } break;
    case QVariant::Type::Char: {
        int8 v = A.getContentAsInteger8Scalar();
        res = v;
    } break;
    case QVariant::Type::String: {
        std::wstring wstr = A.getContentAsWideString();
        res = wstringToQString(wstr);
    } break;
    case QVariant::Type::StringList: {
        wstringVector v = A.getContentAsWideStringVector(true);
        QStringList stringlist;
        for (size_t k = 0; k < v.size(); k++) {
            stringlist << wstringToQString(v[k]);
        }
        res = stringlist;
    } break;
    case QVariant::Type::ByteArray: {
        Dimensions dimsA = A.getDimensions();
        if (!A.isVector()) {
            Error(_W("vector expected."));
        }
        A.promoteType(NLS_INT8);
        int8* arrayInt8 = (int8*)A.getDataPointer();
        QByteArray qbytearray((int)dimsA.getElementCount(), ' ');
        int count = qbytearray.count();
        char* data = qbytearray.data();
        for (int k = 0; k < count; k++) {
            data[k] = (char)arrayInt8[k];
        }
        res = qbytearray;
    } break;
    case QVariant::Type::BitArray: {
        Dimensions dimsA = A.getDimensions();
        if (!A.isVector()) {
            Error(_W("vector expected."));
        }
        A.promoteType(NLS_LOGICAL);
        logical* arrayLogical = (logical*)A.getDataPointer();
        QBitArray qbitarray((int)dimsA.getElementCount());
        int count = qbitarray.count();
        for (int k = 0; k < count; k++) {
            qbitarray[k] = (bool)arrayLogical[k];
        }
        res = qbitarray;
    } break;
    case QVariant::Type::Date: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 3);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x3 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QDate date(arrayInt[0], arrayInt[1], arrayInt[2]);
        res = date;
    } break;
    case QVariant::Type::Time: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QTime time(arrayInt[0], arrayInt[1], arrayInt[2], arrayInt[3]);
        res = time;
    } break;
    case QVariant::Type::DateTime: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 7);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x7 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QDate date(arrayInt[0], arrayInt[1], arrayInt[2]);
        QTime time(arrayInt[3], arrayInt[4], arrayInt[5], arrayInt[6]);
        QDateTime datetime(date, time);
        datetime.setTimeSpec(Qt::UTC); // FORCE UTC
        res = datetime;
    } break;
    case QVariant::Type::Url: {
        std::wstring wstr = A.getContentAsWideString();
        QUrl qurl(wstringToQString(wstr));
        res = qurl;
    } break;
    case QVariant::Type::Rect: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QRect qrect(arrayInt[0], arrayInt[1], arrayInt[2], arrayInt[3]);
        res = qrect;
    } break;
    case QVariant::Type::RectF: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_DOUBLE);
        double* arrayDouble = (double*)A.getDataPointer();
        QRectF qrectf(arrayDouble[0], arrayDouble[1], arrayDouble[2], arrayDouble[3]);
        res = qrectf;
    } break;
    case QVariant::Type::Size: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 2);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x2 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QSize qsize(arrayInt[0], arrayInt[1]);
        res = qsize;
    } break;
    case QVariant::Type::SizeF: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 2);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x2 expected."));
        }
        A.promoteType(NLS_DOUBLE);
        double* arrayDouble = (double*)A.getDataPointer();
        QSizeF qsizef(arrayDouble[0], arrayDouble[1]);
        res = qsizef;
    } break;
    case QVariant::Type::Line: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QLine qline(arrayInt[0], arrayInt[1], arrayInt[2], arrayInt[3]);
        res = qline;
    } break;
    case QVariant::Type::LineF: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_DOUBLE);
        double* arrayDouble = (double*)A.getDataPointer();
        QLineF qlinef(arrayDouble[0], arrayDouble[1], arrayDouble[2], arrayDouble[3]);
        res = qlinef;
    } break;
    case QVariant::Type::Point: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 2);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x2 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QPoint point(arrayInt[0], arrayInt[1]);
        res = point;
    } break;
    case QVariant::Type::PointF: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 2);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x2 expected."));
        }
        A.promoteType(NLS_DOUBLE);
        double* arrayDouble = (double*)A.getDataPointer();
        QPointF pointf(arrayDouble[0], arrayDouble[1]);
        res = pointf;
    } break;
    case QVariant::Type::Uuid: {
        std::wstring wstr = A.getContentAsWideString();
        QUuid quuid(wstringToQString(wstr));
        res = quuid;
    } break;
    case QVariant::Type::Color: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_INT32);
        int* arrayInt = (int*)A.getDataPointer();
        QColor color(arrayInt[0], arrayInt[1], arrayInt[2], arrayInt[3]);
        res = color;
    } break;
    case QVariant::Type::Matrix: {
        QMatrix qmatrix;
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 6);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x6 expected."));
        }
        A.promoteType(NLS_DOUBLE);
        double* arrayDouble = (double*)A.getDataPointer();
        qmatrix.setMatrix(arrayDouble[0], arrayDouble[1], arrayDouble[2], arrayDouble[3],
            arrayDouble[4], arrayDouble[5]);
        res = qmatrix;
    } break;
    case QVariant::Type::Transform: {
        QTransform qtransform;
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(3, 3);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("matrix 3x3 expected."));
        }
        A.promoteType(NLS_DOUBLE);
        double* arrayDouble = (double*)A.getDataPointer();
        qtransform.setMatrix(arrayDouble[0], arrayDouble[1], arrayDouble[2], arrayDouble[3],
            arrayDouble[4], arrayDouble[5], arrayDouble[6], arrayDouble[7], arrayDouble[8]);
        res = qtransform;
    } break;
    case QVariant::Type::Matrix4x4: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(4, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("matrix 4x4 expected."));
        }
        A.promoteType(NLS_SINGLE);
        single* arraySingle = (single*)A.getDataPointer();
        QMatrix4x4 qmatrix4x4;
        single* data = qmatrix4x4.data();
        for (int k = 0; k < 16; k++) {
            data[k] = arraySingle[k];
        }
        res = qmatrix4x4;
    } break;
    case QVariant::Type::Vector2D: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 2);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x2 expected."));
        }
        A.promoteType(NLS_SINGLE);
        single* arraySingle = (single*)A.getDataPointer();
        QVector2D qvector2d(arraySingle[0], arraySingle[1]);
        res = qvector2d;
    } break;
    case QVariant::Type::Vector3D: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 3);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x3 expected."));
        }
        A.promoteType(NLS_SINGLE);
        single* arraySingle = (single*)A.getDataPointer();
        QVector3D qvector3d(arraySingle[0], arraySingle[1], arraySingle[2]);
        res = qvector3d;
    } break;
    case QVariant::Type::Vector4D: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_SINGLE);
        single* arraySingle = (single*)A.getDataPointer();
        QVector4D qvector4d(arraySingle[0], arraySingle[1], arraySingle[2], arraySingle[3]);
        res = qvector4d;
    } break;
    case QVariant::Type::Quaternion: {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsExpected(1, 4);
        if (!dimsA.equals(dimsExpected)) {
            Error(_W("vector 1x4 expected."));
        }
        A.promoteType(NLS_SINGLE);
        single* arraySingle = (single*)A.getDataPointer();
        QQuaternion qq(arraySingle[0], arraySingle[1], arraySingle[2], arraySingle[3]);
        res = qq;
    } break;
    case QVariant::Type::List: {
        if (!A.isCell()) {
            Error(_W("cell expected."));
        }
        res = ArrayOfToQVariant(A);
    } break;
    case QVariant::Type::Map: {
        if (!A.isStruct()) {
            Error(_W("structs expected."));
        }
        res = ArrayOfToQVariant(A);
    } break;
    default: {
        Error(_W("QVariant type not managed."));
    } break;
    }
    return res;
}
//=============================================================================
template <class T>
QVariant
NelsonTypeToQVariant(ArrayOf A)
{
    QVariant res;
    if (A.isVector()) {
        QVariantList qlistVariant;
        Dimensions dimsA = A.getDimensions();
        T* nlsArray = (T*)A.getDataPointer();
        for (size_t k = 0; k < dimsA.getElementCount(); k++) {
            QVariant element = QVariant(nlsArray[k]);
            qlistVariant.push_back(element);
        }
        QVariant res = qlistVariant;
        return res;
    } else {
        QVariantList qlistVariantRows;
        Dimensions dimsA = A.getDimensions();
        T* nlsArray = (T*)A.getDataPointer();
        indexType rows = dimsA.getRows();
        indexType columns = dimsA.getColumns();
        for (indexType i = 0; i < rows; i++) {
            QVariantList qlistVariantColumns;
            for (indexType j = 0; j < columns; j++) {
                size_t idx = i + j * rows;
                qlistVariantColumns.push_back(QVariant(nlsArray[idx]));
            }
            qlistVariantRows.push_back(qlistVariantColumns);
        }
        res = qlistVariantRows;
    }
    return res;
}
//=============================================================================
QVariant
ArrayOfToQVariant(ArrayOf A)
{
    QVariant res;
    if (A.isSparse() || !A.is2D()) {
        Error(_W("Type conversion to QVariant not managed."));
    }
    if (A.isEmpty()) {
        Error(_W("Empty matrix not managed."));
    }
    Class ClassA = A.getDataClass();
    switch (ClassA) {
    case NLS_LOGICAL: {
        if (A.isScalar()) {
            res = ArrayOfToQVariant(A, QVariant::Type::Bool);
        } else {
            res = NelsonTypeToQVariant<logical>(A);
        }
    } break;
    case NLS_UINT8: {
        A.promoteType(NLS_UINT32);
        res = ArrayOfToQVariant(A);
    } break;
    case NLS_INT8: {
        A.promoteType(NLS_INT32);
        res = ArrayOfToQVariant(A);
    } break;
    case NLS_UINT16: {
        A.promoteType(NLS_UINT32);
        res = ArrayOfToQVariant(A);
    } break;
    case NLS_INT16: {
        A.promoteType(NLS_INT32);
        res = ArrayOfToQVariant(A);
    } break;
    case NLS_UINT32: {
        if (A.isScalar()) {
            res = ArrayOfToQVariant(A, QVariant::Type::UInt);
        } else {
            res = NelsonTypeToQVariant<uint32>(A);
        }
    } break;
    case NLS_INT32: {
        if (A.isScalar()) {
            res = ArrayOfToQVariant(A, QVariant::Type::Int);
        } else {
            res = NelsonTypeToQVariant<int>(A);
        }
    } break;
    case NLS_UINT64: {
        if (A.isScalar()) {
            res = ArrayOfToQVariant(A, QVariant::Type::ULongLong);
        } else {
            res = NelsonTypeToQVariant<unsigned long long>(A);
        }
    } break;
    case NLS_INT64: {
        if (A.isScalar()) {
            res = ArrayOfToQVariant(A, QVariant::Type::LongLong);
        } else {
            res = NelsonTypeToQVariant<long long>(A);
        }
    } break;
    case NLS_SINGLE: {
        A.promoteType(NLS_DOUBLE);
        return ArrayOfToQVariant(A);
    } break;
    case NLS_DOUBLE: {
        if (A.isScalar()) {
            res = ArrayOfToQVariant(A, QVariant::Type::Double);
        } else {
            res = NelsonTypeToQVariant<double>(A);
        }
    } break;
    case NLS_CHAR: {
        if (A.isRowVectorCharacterArray()) {
            res = ArrayOfToQVariant(A, QVariant::Type::String);
        } else {
            Error(_W("Type conversion to QVariant not managed."));
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (A.isVector()) {
            wstringVector vstr = A.getContentAsWideStringVector();
            QStringList stringlist;
            for (size_t k = 0; k < vstr.size(); k++) {
                stringlist.push_back(wstringToQString(vstr[k]));
            }
            QVariant res = stringlist;
            return res;
        } else {
            QVariantList qlistVariantRows;
            Dimensions dimsA = A.getDimensions();
            ArrayOf* cellArray = (ArrayOf*)A.getDataPointer();
            indexType rows = dimsA.getRows();
            indexType columns = dimsA.getColumns();
            for (indexType i = 0; i < rows; i++) {
                QVariantList qlistVariantColumns;
                for (indexType j = 0; j < columns; j++) {
                    size_t idx = i + j * rows;
                    QVariant Q = ArrayOfToQVariant(cellArray[idx]);
                    qlistVariantColumns.push_back(Q);
                }
                qlistVariantRows.push_back(qlistVariantColumns);
            }
            res = qlistVariantRows;
        }
    } break;
    case NLS_CELL_ARRAY: {
        if (A.isVector()) {
            if (IsCellOfString(A)) {
                wstringVector vstr = A.getContentAsWideStringVector();
                QStringList stringlist;
                for (size_t k = 0; k < vstr.size(); k++) {
                    stringlist.push_back(wstringToQString(vstr[k]));
                }
                QVariant res = stringlist;
                return res;
            } else {
                Dimensions dimsA = A.getDimensions();
                ArrayOf* cellArray = (ArrayOf*)A.getDataPointer();
                QVariantList qvariantList;
                for (indexType k = 0; k < dimsA.getElementCount(); k++) {
                    qvariantList.push_back(ArrayOfToQVariant(cellArray[k]));
                }
                res = qvariantList;
            }
        } else {
            QVariantList qlistVariantRows;
            Dimensions dimsA = A.getDimensions();
            ArrayOf* cellArray = (ArrayOf*)A.getDataPointer();
            indexType rows = dimsA.getRows();
            indexType columns = dimsA.getColumns();
            for (indexType i = 0; i < rows; i++) {
                QVariantList qlistVariantColumns;
                for (indexType j = 0; j < columns; j++) {
                    size_t idx = i + j * rows;
                    QVariant Q = ArrayOfToQVariant(cellArray[idx]);
                    qlistVariantColumns.push_back(Q);
                }
                qlistVariantRows.push_back(qlistVariantColumns);
            }
            res = qlistVariantRows;
        }
    } break;
    case NLS_STRUCT_ARRAY: {
        QVariantMap qvariantMap;
        stringVector fieldnames = A.getFieldNames();
        for (size_t k = 0; k < fieldnames.size(); k++) {
            qvariantMap[fieldnames[k].c_str()] = ArrayOfToQVariant(A.getField(fieldnames[k]));
        }
        res = qvariantMap;
    } break;
    case NLS_HANDLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    default: {
        Error(_W("Type conversion to QVariant not managed."));
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
