//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Overload.hpp"
#include "FunctionsInMemory.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isObject(NelsonType nelsonType, const std::string& typeName)
{
    return nelsonType == NLS_HANDLE
        || (nelsonType == NLS_STRUCT_ARRAY && typeName != NLS_STRUCT_ARRAY_STR);
}
//=============================================================================
ArrayOf
Evaluator::uplusOperator(const ArrayOf& A)
{
    FunctionDef* funcDef = nullptr;
    NelsonType classA = A.getDataClass();
    std::string destinationTypeName;
    bool isObjectA = A.isClassStruct() || (classA == NLS_HANDLE);
    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            destinationTypeName = A.getHandleCategory();
        } else {
            destinationTypeName = A.getStructType();
        }
    } else {
        destinationTypeName = ClassToString(classA);
    }
    std::string overloadTypeName = overloadOperatorName(destinationTypeName, UPLUS_OP);
    if (A.isSparse()) {
        overloadTypeName = NLS_SPARSE_STR + overloadTypeName;
    }
    if (!this->isOverloadAllowed()) {
        Context* context = this->getContext();
        if (context->lookupFunction(overloadTypeName, funcDef, !this->isOverloadAllowed())) {
            FunctionsInMemory::getInstance()->add(classA, UPLUS_OP, funcDef);
        }
    } else {
        if (isObject(classA, destinationTypeName)) {
            if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
                Context* context = this->getContext();
                context->lookupFunction(overloadTypeName, funcDef);
            }
        } else {
            if (!FunctionsInMemory::getInstance()->findUnaryOperator(classA, UPLUS_OP, funcDef)) {
                Context* context = this->getContext();
                if (context->lookupFunction(overloadTypeName, funcDef)) {
                    FunctionsInMemory::getInstance()->add(classA, UPLUS_OP, funcDef);
                }
            }
        }
    }

    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }
    ArrayOfVector argsIn;
    argsIn << A;
    ArrayOfVector r = funcDef->evaluateFunction(this, argsIn, 1);
    return r[0];
}
//=============================================================================
}
//=============================================================================
