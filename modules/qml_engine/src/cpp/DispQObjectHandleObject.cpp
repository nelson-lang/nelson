//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QBitArray>
#include <QtCore/QDateTime>
#include <QtCore/QLine>
#include <QtCore/QLineF>
#include <QtCore/QRect>
#include <QtCore/QStringList>
#include <QtCore/QUrl>
#include <QtCore/QUuid>
#include <QtGui/QColor>
#include <QtGui/QMatrix4x4>
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QMatrix>
#endif
#include <QtGui/QQuaternion>
#include <QtGui/QTransform>
#include <QtGui/QVector2D>
#include <QtQml/QQmlComponent>
#include "DispQObjectHandleObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include "fieldnamesQObjectHandleObject.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
dispParent(QObject* qobj, std::wstring& msg)
{
    QObject* parent = qobj->parent();
    std::wstring line;
    if (parent) {
        line = L"\t" + utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR) + L": " + L"handle" + L"\n";
    }
    msg = msg + line;
}
//=============================================================================
static void
dispChildren(QObject* qobj, std::wstring& msg)
{
    QObjectList childs = qobj->children();
    int s = childs.size();
    std::wstring line;
    if (s > 0) {
        line = L"\t" + utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR) + L": " + L"handle 1x"
            + std::to_wstring(s) + L"\n";
        msg = msg + line;
    }
}
//=============================================================================
static void
dispQRect(const QRect& qrect, const std::wstring& fieldname, std::wstring& msg)
{
    int x = qrect.x();
    int y = qrect.y();
    int w = qrect.width();
    int h = qrect.height();
    std::wstring wstr = L"x:" + std::to_wstring(x) + L" " + L"y:" + std::to_wstring(y) + L" "
        + L"w:" + std::to_wstring(w) + L" " + L"h:" + std::to_wstring(h);
    msg = msg + L"\t" + fieldname + L": " + wstr + L"\n";
}
//=============================================================================
static void
dispQRectF(const QRectF& qrectf, const std::wstring& fieldname, std::wstring& msg)
{
    double x = qrectf.x();
    double y = qrectf.y();
    double w = qrectf.width();
    double h = qrectf.height();
    double intpart;
    std::wstring wstr;
    std::wstring wstr_x;
    std::wstring wstr_y;
    std::wstring wstr_w;
    std::wstring wstr_h;
    if (std::modf(x, &intpart) == 0.0) {
        wstr_x = std::to_wstring(int(x));
    } else {
        wstr_x = std::to_wstring(x);
    }
    if (std::modf(y, &intpart) == 0.0) {
        wstr_y = std::to_wstring(int(y));
    } else {
        wstr_y = std::to_wstring(y);
    }
    if (std::modf(w, &intpart) == 0.0) {
        wstr_w = std::to_wstring(int(w));
    } else {
        wstr_w = std::to_wstring(w);
    }
    if (std::modf(h, &intpart) == 0.0) {
        wstr_h = std::to_wstring(int(h));
    } else {
        wstr_h = std::to_wstring(h);
    }
    wstr = L"x:" + wstr_x + L" " + L"y:" + wstr_y + L" " + L"w:" + wstr_w + L" " + L"h:" + wstr_h;
    msg = msg + L"\t" + fieldname + L": " + L"QRectF" + L" " + wstr + L"\n";
}
//=============================================================================
static void
dispQPoint(QPoint qpoint, const std::wstring& fieldname, std::wstring& msg)
{
    int x = qpoint.x();
    int y = qpoint.y();
    std::wstring wstr = L"x:" + std::to_wstring(x) + L" " + L"y:" + std::to_wstring(y);
    msg = msg + L"\t" + fieldname + L": " + wstr + L"\n";
}
//=============================================================================
static void
dispQPointF(const QPointF& qpointf, const std::wstring& fieldname, std::wstring& msg)
{
    double x = qpointf.x();
    double y = qpointf.y();
    double intpart;
    std::wstring wstr;
    if (std::modf(x, &intpart) == 0.0 && std::modf(y, &intpart) == 0.0) {
        wstr = L"x:" + std::to_wstring(int(x)) + L" " + L"y:" + std::to_wstring(int(y));
    } else {
        wstr = L"x:" + std::to_wstring(x) + L" " + L"y:" + std::to_wstring(y);
    }
    msg = msg + L"\t" + fieldname + L": " + L"QRectF" + L" " + wstr + L"\n";
}
//=============================================================================
static void
dispQColor(const QColor& qcolor, const std::wstring& fieldname, std::wstring& msg)
{
    int r = qcolor.red();
    int g = qcolor.green();
    int b = qcolor.blue();
    int a = qcolor.alpha();
    std::wstring wstr = L"r:" + std::to_wstring(r) + L" " + L"g:" + std::to_wstring(g) + L" "
        + L"b:" + std::to_wstring(b) + L" " + L"a:" + std::to_wstring(a);
    msg = msg + L"\t" + fieldname + L": " + L"QColor" + L" " + wstr + L"\n";
}
//=============================================================================
static void
DispQObjectHandleObject(Interface* io, QObjectHandleObject* qmlHandle)
{
    if (qmlHandle != nullptr) {
        wstringVector wfieldnames;
        QObject* qobj = (QObject*)qmlHandle->getPointer();
        if (qobj) {
            fieldnamesQObjectHandleObject(qmlHandle, false, wfieldnames);
            std::wstring msg;
            for (const std::wstring& wfieldname : wfieldnames) {
                if (wfieldname == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
                    dispParent(qobj, msg);
                } else if (wfieldname == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
                    dispChildren(qobj, msg);
                } else if (wfieldname == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
                    std::string name = qobj->metaObject()->className();
                    msg = msg + L"\t" + wfieldname + L": " + utf8_to_wstring(name) + L"\n";
                } else {
                    QVariant propertyValue = qobj->property(wstring_to_utf8(wfieldname).c_str());
                    if (propertyValue.isValid()) {
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
                        QMetaType metaType = propertyValue.metaType();
                        int id = metaType.id();
#else
                        int id = QMetaType::type(propertyValue.typeName());
#endif
                        switch (id) {
                        case QMetaType::Type::QRect: {
                            QRect qrect = propertyValue.toRect();
                            dispQRect(qrect, wfieldname, msg);
                        } break;
                        case QMetaType::Type::QRectF: {
                            QRectF qrect = propertyValue.toRectF();
                            dispQRectF(qrect, wfieldname, msg);
                        } break;
                        case QMetaType::Type::QPoint: {
                            QPoint qpoint = propertyValue.toPoint();
                            dispQPoint(qpoint, wfieldname, msg);
                        } break;
                        case QMetaType::Type::QPointF: {
                            QPointF qpointf = propertyValue.toPointF();
                            dispQPointF(qpointf, wfieldname, msg);
                        } break;
                        case QMetaType::Type::QColor: {
                            QColor qcolor = qvariant_cast<QColor>(propertyValue);
                            dispQColor(qcolor, wfieldname, msg);
                        } break;
                        case QMetaType::Type::Bool:
                        case QMetaType::Type::Int:
                        case QMetaType::Type::UInt:
                        case QMetaType::Type::LongLong:
                        case QMetaType::Type::ULongLong:
                        case QMetaType::Type::Double:
                        case QMetaType::Type::Char:
                        case QMetaType::Type::QString:
                        case QMetaType::Type::QStringList:
                        case QMetaType::Type::QByteArray:
                        case QMetaType::Type::QBitArray:
                        case QMetaType::Type::QDate:
                        case QMetaType::Type::QTime:
                        case QMetaType::Type::QDateTime:
                        case QMetaType::Type::QUrl:
                        case QMetaType::Type::QSize:
                        case QMetaType::Type::QSizeF:
                        case QMetaType::Type::QLine:
                        case QMetaType::Type::QLineF:
                        case QMetaType::Type::QUuid:
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
                        case QMetaType::Type::QMatrix:
#endif
                        case QMetaType::Type::QTransform:
                        case QMetaType::Type::QMatrix4x4:
                        case QMetaType::Type::QVector2D:
                        case QMetaType::Type::QVector3D:
                        case QMetaType::Type::QVector4D:
                        case QMetaType::Type::QQuaternion:
                        default: {
                            if (propertyValue.canConvert<QString>()) {
                                msg = msg + L"\t" + wfieldname + L": "
                                    + QStringTowstring(propertyValue.typeName()) + L" "
                                    + QStringTowstring(propertyValue.toString()) + L"\n";
                            } else {
                                msg = msg + L"\t" + wfieldname + L": "
                                    + QStringTowstring(propertyValue.typeName()) + L" " + L"handle"
                                    + L"\n";
                            }
                        } break;
                        }
                    }
                }
            }
            if (!msg.empty()) {
                io->outputMessage(msg);
            }
        } else {
            std::wstring msg = L"  " + _W("handle to deleted: ") + QOBJECT_CATEGORY_STR + L"\n";
            io->outputMessage(msg);
        }
    }
}
//=============================================================================
void
DispQObjectHandleObject(Interface* io, const ArrayOf& A, const std::string& name)
{
    if (A.isHandle()) {
        DisplayVariableHeader(io, A, utf8_to_wstring(name), false);
        if (A.isScalar()) {
            if (A.getHandleCategory() != QOBJECT_CATEGORY_STR) {
                Error(_W("QObject handle expected."));
            }

            io->outputMessage("\n");

            QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)A.getContentAsHandleScalar();
            DispQObjectHandleObject(io, qmlhandleobj);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("QObject handle expected."));
    }
}
//=============================================================================
}
//=============================================================================
