//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstring>
#include "mex.h"
#include "MxObject.h"
#include "MxCall.h"
#include "MxTypes.h"
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "ClassName.hpp"
#include "MxArrayOf.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
//=============================================================================
bool
mxIsClass(const mxArray* pm, const char* classname)
{
    bool res = false;
    if (pm == nullptr) {
        return false;
    }
    if (strcmp(classname, "cell") == 0) {
        return (pm->classID == mxCELL_CLASS);
    }
    if (strcmp(classname, "char") == 0) {
        return (pm->classID == mxCHAR_CLASS);
    }
    if (strcmp(classname, "double") == 0) {
        return (pm->classID == mxDOUBLE_CLASS);
    }
    if (strcmp(classname, "function_handle") == 0) {
        return (pm->classID == mxFUNCTION_CLASS);
    }
    if (strcmp(classname, "int8") == 0) {
        return (pm->classID == mxINT8_CLASS);
    }
    if (strcmp(classname, "int16") == 0) {
        return (pm->classID == mxINT16_CLASS);
    }
    if (strcmp(classname, "int32") == 0) {
        return (pm->classID == mxINT32_CLASS);
    }
    if (strcmp(classname, "int64") == 0) {
        return (pm->classID == mxINT64_CLASS);
    } else if (strcmp(classname, "logical") == 0) {
        return (pm->classID == mxLOGICAL_CLASS);
    } else if (strcmp(classname, "single") == 0) {
        return (pm->classID == mxSINGLE_CLASS);
    } else if (strcmp(classname, "struct") == 0) {
        return (pm->classID == mxSTRUCT_CLASS);
    } else if (strcmp(classname, "uint8") == 0) {
        return (pm->classID == mxUINT8_CLASS);
    } else if (strcmp(classname, "uint16") == 0) {
        return (pm->classID == mxUINT16_CLASS);
    } else if (strcmp(classname, "uint32") == 0) {
        return (pm->classID == mxUINT32_CLASS);
    } else if (strcmp(classname, "uint64") == 0) {
        return (pm->classID == mxUINT64_CLASS);
    } else if (strcmp(classname, "unknown") == 0) {
        return (pm->classID == mxUNKNOWN_CLASS);
    } else {
        Nelson::ArrayOf nlsArrayOf = Nelson::MxArrayToArrayOf(pm);
        std::string name = Nelson::ClassName(nlsArrayOf);
        res = strcmp(classname, name.c_str()) == 0;
    }
    return res;
}
//=============================================================================
mxClassID
mxGetClassID(const mxArray* pm)
{
    if (pm != nullptr) {
        return (mxClassID)pm->classID;
    }
    return mxUNKNOWN_CLASS;
}
//=============================================================================
static char* mxClassName = nullptr;
//=============================================================================
const char*
mxGetClassName(const mxArray* pm)
{
    if (mxClassName != nullptr) {
        free(mxClassName);
        mxClassName = nullptr;
    }
    if (pm != nullptr) {
        Nelson::ArrayOf nlsArrayOf = Nelson::MxArrayToArrayOf(pm);
        std::string name = Nelson::ClassName(nlsArrayOf);
        mxClassName = (char*)calloc(sizeof(char), name.size() + 1);
        if (mxClassName) {
            strcpy(mxClassName, name.c_str());
        }
        return mxClassName;
    }
    return nullptr;
}
//=============================================================================
int
mxSetClassName(mxArray* array_ptr, const char* classname)
{
    if (array_ptr != nullptr) {
        if (array_ptr->ptr != nullptr) {
            auto* ptr = (Nelson::ArrayOf*)array_ptr->ptr;
            if (ptr->isStruct()) {
                ptr->promoteType(Nelson::NLS_CLASS_ARRAY);
                ptr->setClassType(classname);
                return 0;
            }
            return -3;
        }
        return -2;
    }
    return -1;
}
//=============================================================================
mxArray*
mxGetProperty(const mxArray* pa, mwIndex index, const char* propname)
{
    mxArray* res = nullptr;
    if (pa != nullptr) {
        if (pa->ptr != nullptr) {
            auto* ptr = (Nelson::ArrayOf*)pa->ptr;
            bool isSupportedObject
                = ptr->isClassType() || ptr->isHandle() || ptr->isGraphicsObject();
            if (!isSupportedObject) {
                mexErrMsgTxt(_("mxGetProperty object expected.").c_str());
            }
            Nelson::Dimensions dims = ptr->getDimensions();
            if (index > dims.getElementCount()) {
                return res;
            }
            Nelson::ArrayOf obj;
            if (index == 0) {
                obj = *ptr;
            } else {
                Nelson::ArrayOf* elements = (Nelson::ArrayOf*)ptr->getDataPointer();
                obj = elements[index];
            }
            Nelson::Evaluator* mainEvaluator
                = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()
                      ->getMainEvaluator();
            if (mainEvaluator != nullptr) {
                Nelson::Context* context = mainEvaluator->getContext();
                if (context != nullptr) {
                    Nelson::FunctionDef* funcDef = nullptr;
                    if (context->lookupFunction("get", funcDef)) {
                        if (funcDef != nullptr) {
                            Nelson::ArrayOfVector argIn;
                            Nelson::ArrayOfVector argOut;
                            argIn.push_back(obj);
                            argIn.push_back(Nelson::ArrayOf::characterArrayConstructor(propname));
                            try {
                                argOut = funcDef->evaluateFunction(mainEvaluator, argIn, 1);
                            } catch (Nelson::Exception&) {
                                return nullptr;
                            }
                            if (!argOut.empty()) {
                                res = Nelson::ArrayOfToMxArray(argOut[0], pa->interleavedcomplex);
                            }
                        }
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
void
mxSetProperty(mxArray* pa, mwIndex index, const char* propname, const mxArray* value)
{
    if (pa != nullptr) {
        if (pa->ptr != nullptr) {
            auto* ptr = (Nelson::ArrayOf*)pa->ptr;
            bool isSupportedObject
                = ptr->isClassType() || ptr->isHandle() || ptr->isGraphicsObject();
            if (!isSupportedObject) {
                mexErrMsgTxt(_("mxSetProperty object expected.").c_str());
            }
            Nelson::Dimensions dims = ptr->getDimensions();
            if (index <= dims.getElementCount()) {
                Nelson::ArrayOf obj;
                if (index == 0) {
                    obj = *ptr;
                } else {
                    Nelson::ArrayOf* elements = (Nelson::ArrayOf*)ptr->getDataPointer();
                    obj = elements[index];
                }
                Nelson::Evaluator* mainEvaluator
                    = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()
                          ->getMainEvaluator();
                if (mainEvaluator != nullptr) {
                    Nelson::Context* context = mainEvaluator->getContext();
                    if (context != nullptr) {
                        Nelson::FunctionDef* funcDef = nullptr;
                        if (context->lookupFunction("set", funcDef)) {
                            if (funcDef != nullptr) {
                                Nelson::ArrayOfVector argIn;
                                Nelson::ArrayOfVector argOut;
                                argIn.push_back(obj);
                                argIn.push_back(
                                    Nelson::ArrayOf::characterArrayConstructor(propname));
                                argIn.push_back(Nelson::MxArrayToArrayOf(value));
                                try {
                                    argOut = funcDef->evaluateFunction(mainEvaluator, argIn, 0);
                                } catch (Nelson::Exception&) {
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
//=============================================================================
bool
mxIsFunctionHandle(const mxArray* pa)
{
    return mxIsClass(pa, "function_handle");
}
//=============================================================================
bool
mxIsOpaque(const mxArray* pa)
{
    if (pa != nullptr) {
        return (pa->classID == mxOPAQUE_CLASS);
    }
    return false;
}
//=============================================================================
bool
mxIsObject(const mxArray* pa)
{
    if (pa != nullptr) {
        return (pa->classID == mxOBJECT_CLASS);
    }
    return false;
}
//=============================================================================
