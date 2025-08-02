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
    call(const QString& functionName, const QString& argString);
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
