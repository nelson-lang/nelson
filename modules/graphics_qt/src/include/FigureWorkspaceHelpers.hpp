//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtCore/QJsonArray>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QRegularExpression>
#include <QtCore/Qt>
#include <cmath>
#include <deque>
//=============================================================================
namespace Nelson {
// Item-data roles used by the property inspector to drive the editor delegate
// from the shared property model (kind/choices/writable) instead of hardcoding
// per-property behaviour.
enum FigurePropertyRole
{
    RolePropertyKind = Qt::UserRole + 1,
    RolePropertyChoices = Qt::UserRole + 2,
    RolePropertyValue = Qt::UserRole + 3,
    RolePropertyWritable = Qt::UserRole + 4,
    RolePropertyDisplay = Qt::UserRole + 5,
};
//=============================================================================
inline void
enqueueFigureCommand(std::deque<QJsonObject>& commands, QJsonObject request)
{
    const auto kind = request["action"].toString();
    if (!commands.empty()) {
        auto& last = commands.back();
        if (last["action"] == request["action"] && last["axesHandle"] == request["axesHandle"]
            && last["historyGroup"] == request["historyGroup"]) {
            if (kind == "pan") {
                request["dxFrac"] = last["dxFrac"].toDouble() + request["dxFrac"].toDouble();
                request["dyFrac"] = last["dyFrac"].toDouble() + request["dyFrac"].toDouble();
            } else if (kind == "zoom") {
                request["factor"] = last["factor"].toDouble() * request["factor"].toDouble();
            }
            if (kind == "pan" || kind == "view" || kind == "zoom" || kind == "state") {
                last = request;
                return;
            }
        }
    }
    commands.push_back(request);
}
//=============================================================================
inline QJsonArray
figureObjectArray(const QJsonValue& value)
{
    return value.isArray() ? value.toArray()
        : value.isObject() ? QJsonArray { value }
                           : QJsonArray {};
}
//=============================================================================
// Compact signature of the figure's object structure (axes + plotted-object
// handles) used by the property inspector to detect when the object browser
// must be rebuilt, without comparing full state.
inline QByteArray
structureSignatureFromState(const QJsonObject& state)
{
    QByteArray sig;
    for (const auto& value : state["axes"].toArray()) {
        const auto axis = value.toObject();
        sig += QByteArray::number(static_cast<qlonglong>(axis["handle"].toDouble())) + ';';
        for (const auto& object : figureObjectArray(axis["objects"])) {
            sig += QByteArray::number(
                       static_cast<qlonglong>(object.toObject()["handle"].toDouble()))
                + ',';
        }
    }
    return sig;
}
//=============================================================================
inline QString
figurePropertyText(const QJsonValue& value)
{
    if (value.isString()) {
        return value.toString();
    }
    if (value.isDouble()) {
        return QString::number(value.toDouble(), 'g', 16);
    }
    if (value.isArray()) {
        return QString::fromUtf8(QJsonDocument(value.toArray()).toJson(QJsonDocument::Compact));
    }
    return {};
}
//=============================================================================
inline QJsonValue
parseFigureProperty(
    const QString& text, const QJsonValue& previous, bool& valid, bool color = false)
{
    valid = true;
    if (color && text.contains(QRegularExpression("[^0-9eE.,+\\-\\[\\]\\s]"))) {
        return text.trimmed();
    }
    if (previous.isString() && !color) {
        return text;
    }
    if (previous.isDouble()) {
        const double number = text.toDouble(&valid);
        valid = valid && std::isfinite(number);
        return number;
    }
    QString input = text.trimmed();
    if (!input.startsWith('[')) {
        input = '[' + input + ']';
    }
    input.replace(QRegularExpression("[,\\s]+"), ",");
    input.replace("[,", "[");
    input.replace(",]", "]");
    QJsonParseError error;
    const auto document = QJsonDocument::fromJson(input.toUtf8(), &error);
    valid = error.error == QJsonParseError::NoError && document.isArray();
    const auto values = document.array();
    if (color) {
        valid = valid && values.size() == 3;
    } else if (previous.isArray()) {
        valid = valid && values.size() == previous.toArray().size();
    }
    for (const auto& value : values) {
        valid = valid && value.isDouble() && std::isfinite(value.toDouble());
    }
    return values;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
