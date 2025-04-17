//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QDebug>
#include "nelsonObject.h"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "Interface.hpp"
#include "ProcessEvents.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
nelsonObject::nelsonObject(QObject* parent) : QObject(parent)
{
    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    if (eval == nullptr) {
        eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    }
}
//=============================================================================
void
nelsonObject::disp(QString msg)
{
    call("disp", msg);
}
//=============================================================================
void
nelsonObject::evaluate(QString msg)
{
    call("execstr", msg);
}
//=============================================================================
void
nelsonObject::processevent()
{
    ProcessEvents();
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariant& arg1)
{
    QVariantList qArgs;
    qArgs.push_back(arg1);
    return call(functionName, qArgs);
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariant& arg1, const QVariant& arg2)
{
    QVariantList qArgs;
    qArgs.push_back(arg1);
    qArgs.push_back(arg2);
    return call(functionName, qArgs);
}
//=============================================================================
QVariant
nelsonObject::call(
    const QString& functionName, const QVariant& arg1, const QVariant& arg2, const QVariant& arg3)
{
    QVariantList qArgs;
    qArgs.push_back(arg1);
    qArgs.push_back(arg2);
    qArgs.push_back(arg3);
    return call(functionName, qArgs);
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
    const QVariant& arg3, const QVariant& arg4)
{
    QVariantList qArgs;
    qArgs.push_back(arg1);
    qArgs.push_back(arg2);
    qArgs.push_back(arg3);
    qArgs.push_back(arg4);
    return call(functionName, qArgs);
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
    const QVariant& arg3, const QVariant& arg4, const QVariant& arg5)
{
    QVariantList qArgs;
    qArgs.push_back(arg1);
    qArgs.push_back(arg2);
    qArgs.push_back(arg3);
    qArgs.push_back(arg4);
    qArgs.push_back(arg5);
    return call(functionName, qArgs);
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariant& arg1, const QVariant& arg2,
    const QVariant& arg3, const QVariant& arg4, const QVariant& arg5, const QVariant& arg6)
{
    qCritical() << "Too many input arguments.";
    return {};
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName)
{
    return call(functionName, QVariantList());
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QString& argString)
{
    return call(functionName, QVariant(argString));
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariantList& args)
{
    QVariant res;
    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    if (eval) {
        std::wstring wfunctionName = QStringTowstring(functionName);
        std::string ufunctionName = wstring_to_utf8(wfunctionName);
        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        if (context->lookupFunction(ufunctionName, funcDef)) {
            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                ArrayOfVector argIn;
                for (const auto& arg : args) {
                    argIn.push_back(QVariantToArrayOf(arg));
                }
                int nLhs = funcDef->outputArgCount();
                ArrayOfVector resVector;
                try {
                    resVector = funcDef->evaluateFunction(eval, argIn, nLhs);
                } catch (const Exception&) {
                    qCritical() << "error function.";
                    return {};
                }
                if (resVector.size() == 0) {
                    res = QVariant();
                } else if (resVector.size() == 1) {
                    res = ArrayOfToQVariant(resVector[0]);
                } else {
                    QVariantList qlistVariant;
                    for (auto& j : resVector) {
                        qlistVariant.push_back(ArrayOfToQVariant(j));
                    }
                    res = qlistVariant;
                }
            } else {
                qCritical() << "function not found.";
                return {};
            }
        } else {
            qCritical() << "function not found.";
            return {};
        }
    }
    return res;
}
//=============================================================================
