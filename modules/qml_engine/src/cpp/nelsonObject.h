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
#include <QtCore/QObject>
#include <QtCore/QVariant>
//=============================================================================
class nelsonObject : public QObject
{
    Q_OBJECT
public:
    explicit nelsonObject(QObject* parent = 0);

signals:
public:
    Q_INVOKABLE void
    disp(QString msg);
    Q_INVOKABLE void
    evaluate(QString msg);
    Q_INVOKABLE void
    processevent();
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariantList& args);
    Q_INVOKABLE QVariant
    call(const QString& functionName);
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariant& arg1);
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariant& arg1, const QVariant& arg2);
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
        const QVariant& arg3);
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
        const QVariant& arg3, const QVariant& arg4);
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
        const QVariant& arg3, const QVariant& arg4, const QVariant& arg5);
    Q_INVOKABLE QVariant
    call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
        const QVariant& arg3, const QVariant& arg4, const QVariant& arg5, const QVariant& arg6);
};
//=============================================================================
