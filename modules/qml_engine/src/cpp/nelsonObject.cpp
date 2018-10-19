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
#include "nelsonObject.h"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "Interface.hpp"
#include "ProcessEvents.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QDebug>
//=============================================================================
using namespace Nelson;
//=============================================================================
static Evaluator* eval = nullptr;
//=============================================================================
nelsonObject::nelsonObject(QObject* parent) : QObject(parent)
{
    if (eval == nullptr) {
        eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
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
    return QVariant();
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName)
{
    return call(functionName, QVariantList());
}
//=============================================================================
QVariant
nelsonObject::call(const QString& functionName, const QVariantList& args)
{
    QVariant res;
    if (eval) {
        std::wstring wfunctionName = QStringTowstring(functionName);
        std::string ufunctionName = wstring_to_utf8(wfunctionName);
        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        if (context->lookupFunction(ufunctionName, funcDef)) {
            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                ArrayOfVector argIn;
                for (int k = 0; k < args.size(); k++) {
                    argIn.push_back(QVariantToArrayOf(args[k]));
                }
                int nLhs = funcDef->outputArgCount();
                ArrayOfVector resVector;
                try {
                    resVector = funcDef->evaluateFunction(eval, argIn, nLhs);
                } catch (const Exception&) {
                    qCritical() << "error function.";
                    return QVariant();
                }
                if (resVector.size() == 0) {
                    res = QVariant();
                } else if (resVector.size() == 1) {
                    res = ArrayOfToQVariant(resVector[0]);
                } else {
                    QVariantList qlistVariant;
                    for (int j = 0; j < resVector.size(); j++) {
                        qlistVariant.push_back(ArrayOfToQVariant(resVector[j]));
                    }
                    res = qlistVariant;
                }
            } else {
                qCritical() << "function not found.";
                return QVariant();
            }
        } else {
            qCritical() << "function not found.";
            return QVariant();
        }
    }
    return res;
}
//=============================================================================
