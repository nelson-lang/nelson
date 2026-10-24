//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "MainThreadTaskQueue.hpp"
#include "RuntimeDispatch.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
qmlCallbackShouldMarshalToEvaluator();
static QVariant
qmlInvokeNelsonFunction(
    Evaluator* eval, const std::string& functionName, const ArrayOfVector& argIn);
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
    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    if (eval == nullptr) {
        return {};
    }
    std::wstring wfunctionName = QStringTowstring(functionName);
    std::string ufunctionName = wstring_to_utf8(wfunctionName);

    // Pure data conversion, safe on whichever thread the QML signal fired on.
    ArrayOfVector argIn;
    for (const auto& arg : args) {
        argIn.push_back(QVariantToArrayOf(arg));
    }

    if (qmlCallbackShouldMarshalToEvaluator()) {
        // Desktop GUI mode: the QML signal fires on the Qt GUI thread while the evaluator
        // runs on a worker thread. Running the interpreter here would drive the same
        // Evaluator/Context from two threads and hang the session, so post the call to the
        // evaluator thread (the REPL drains the queue at the next statement boundary).
        // Fire-and-forget: asynchronous QML handlers (e.g. MouseArea.onPositionChanged)
        // discard the return value.
        MainThreadTaskQueue::getInstance()->postToInterpreterThread(
            [ufunctionName, argIn](Evaluator* evaluator) {
                qmlInvokeNelsonFunction(evaluator, ufunctionName, argIn);
            });
        return {};
    }

    // On the evaluator thread (terminal modes, or QML called from Nelson code): run in
    // place so the result reaches the QML caller.
    return qmlInvokeNelsonFunction(eval, ufunctionName, argIn);
}
//=============================================================================
static bool
qmlCallbackShouldMarshalToEvaluator()
{
    // The evaluator runs on a worker thread only in the desktop and web GUI modes; in
    // terminal modes a QML handler already runs on it.
    const int mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    const bool evaluatorRunsOnWorkerThread
        = (mode == NELSON_ENGINE_MODE::GUI) || (mode == NELSON_ENGINE_MODE::WEB_GUI);
    if (!evaluatorRunsOnWorkerThread) {
        return false;
    }
    // Marshal only from the GUI thread. When QML is driven from Nelson code the signal
    // fires on the evaluator thread itself, and running in place keeps the return value.
    return Nelson::isGuiThread();
}
//=============================================================================
static QVariant
qmlInvokeNelsonFunction(
    Evaluator* eval, const std::string& functionName, const ArrayOfVector& argIn)
{
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    if (!context->lookupFunction(functionName, funcDef)) {
        qCritical() << "Function not found.";
        return {};
    }
    if ((funcDef->type() != NLS_BUILT_IN_FUNCTION) && (funcDef->type() != NLS_MACRO_FUNCTION)) {
        qCritical() << "Function not found.";
        return {};
    }
    int nLhs = funcDef->outputArgCount();
    ArrayOfVector resVector;
    try {
        resVector = funcDef->evaluateFunction(eval, argIn, nLhs);
    } catch (const NelsonException&) {
        qCritical() << "error function.";
        return {};
    }
    if (resVector.size() == 0) {
        return {};
    }
    if (resVector.size() == 1) {
        return ArrayOfToQVariant(resVector[0]);
    }
    QVariantList qlistVariant;
    for (auto& j : resVector) {
        qlistVariant.push_back(ArrayOfToQVariant(j));
    }
    return qlistVariant;
}
//=============================================================================
