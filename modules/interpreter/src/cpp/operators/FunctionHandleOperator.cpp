//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::functionHandleNamedOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    std::string ident = t->down->text;
    AnonymousMacroFunctionDef* cp = nullptr;
    try {
        cp = new AnonymousMacroFunctionDef(ident);
    } catch (std::bad_alloc&) {
        cp = nullptr;
    } catch (Exception&) {
        delete cp;
        cp = nullptr;
    }
    function_handle functionHandle;
    if (cp) {
        functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
    } else {
        Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
    }
    ArrayOf retval = ArrayOf::functionHandleConstructor(functionHandle);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::functionHandleAnonymousOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    stringVector arguments;
    AbstractSyntaxTreePtr code;
    if (t->down->right == nullptr) {
        code = t->down;
    } else {
        arguments = t->down->toStringList();
        code = t->down->right;
    }
    std::string content = code->toString(true);

    stringVector variableNames;
    std::vector<ArrayOf> variables;

    Scope* scope = getContext()->getCurrentScope();
    if (scope != nullptr) {
        scope->getVariablesList(false, variableNames);
    }
    variables.reserve(variableNames.size());
    for (auto name : variableNames) {
        ArrayOf variable;
        if (scope->lookupVariable(name, variable)) {
            variables.push_back(variable);
        }
    }
    AnonymousMacroFunctionDef* cp = nullptr;
    try {
        cp = new AnonymousMacroFunctionDef(content, arguments, variableNames, variables);
    } catch (std::bad_alloc&) {
        cp = nullptr;
    } catch (Exception&) {
        delete cp;
        cp = nullptr;
    }
    function_handle functionHandle;
    if (cp) {
        functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
    } else {
        Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
    }
    ArrayOf retval = ArrayOf::functionHandleConstructor(functionHandle);
    callstack.popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
