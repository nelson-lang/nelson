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
#include <functional>
#include "Evaluator.hpp"
#include "Profiler.hpp"
#include "IsValidVariableName.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "Operators.hpp"
#include "characters_encoding.hpp"
#include "ClassName.hpp"
#include "PredefinedErrorMessages.hpp"
#include "OverloadName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
Evaluator::assignStatement(AbstractSyntaxTreePtr t, bool printIt)
{
    uint64 ticProfiling = Profiler::getInstance()->tic();
    ArrayOf b = expression(t->right);
    std::string variableName = t->text;
    if (t->down != nullptr) {
        b = assignExpression(t, b);
    }
    const bool dropResult = isPlaceholderIdentifier(variableName);
    if (!dropResult) {
        ArrayOf* var = context->lookupVariable(variableName);
        if (var == nullptr) {
            b.ensureSingleOwner();
            bool bInserted = context->insertVariable(variableName, b);
            if (!bInserted) {
                if (IsValidVariableName(variableName, true)) {
                    Error(_W("Redefining permanent variable."));
                }
                Error(_W("Valid variable name expected."));
            }
        } else {
            if (context->isLockedVariable(variableName)) {
                Error(_W("Redefining permanent variable."));
            }
            var->setValue(b);
        }
        if (printIt) {
            display(b, variableName, false, false);
        }
    }
    if (ticProfiling != 0) {
        internalProfileFunction stack = computeProfileStack(
            this, SUBSASGN_OPERATOR_STR, utf8_to_wstring(callstack.getLastContext()));
        Profiler::getInstance()->toc(ticProfiling, stack);
    }
}
//=============================================================================
ArrayOf
Evaluator::assignExpression(AbstractSyntaxTreePtr t, ArrayOf& val)
{
    ArrayOfVector vec;
    vec.push_back(val);
    return assignExpression(t, vec);
}
//=============================================================================
// If we got this far, we must have at least one subindex
ArrayOf
Evaluator::assignExpression(AbstractSyntaxTreePtr t, ArrayOfVector& value)
{
    callstack.pushID((size_t)t->getContext());
    if (t->down == nullptr) {
        ArrayOf retval(value[0]);
        value.pop_front();
        callstack.popID();
        return retval;
    }
    // Get the variable in question
    ArrayOf* var = context->lookupVariable(t->text);
    ArrayOf lhs;
    ArrayOf data;
    if (var == nullptr) {
        lhs = ArrayOf::emptyConstructor();
        data = lhs;
    } else {
        lhs.setValue(*var);
        data.setValue(*var);
    }
    // Set up a stack
    ArrayOfVector stack;
    AbstractSyntaxTreePtrVector ref;
    AbstractSyntaxTreePtr s = t->down;
    // Subindex
    while (s->right != nullptr) {
        if (!data.isEmpty()) {
            data = simpleSubindexExpression(data, s);
        }
        stack.push_back(data);
        ref.push_back(s);
        s = s->right;
    }
    // Do the assignment on the last temporary
    ArrayOf tmp(data);
    simpleAssign(tmp, s, value);
    ArrayOf rhs(tmp);
    if (stack.size() > 0) {
        stack.pop_back();
        // Now we have to "unwind" the stack
        while (stack.size() > 0) {
            // Grab the next temporary off the stack
            tmp = stack.back();
            // Make the assignment
            simpleAssign(tmp, ref.back(), rhs);
            // Assign this temporary to be the RHS of the next temporary
            rhs = tmp;
            // Pop the two stacks
            stack.pop_back();
            ref.pop_back();
        }
        // Complete the final assignment:
        // Last assignment is to lhs
        simpleAssign(lhs, ref.back(), tmp);
    } else {
        lhs = tmp;
    }
    callstack.popID();
    return lhs;
}
//=============================================================================
void
Evaluator::simpleAssign(ArrayOf& r, AbstractSyntaxTreePtr t, ArrayOf& value)
{
    ArrayOfVector vec;
    vec.push_back(value);
    simpleAssign(r, t, vec);
}
//=============================================================================
ArrayOfVector
Evaluator::simpleAssignClass(const ArrayOf& r, const stringVector& subtypes,
    const ArrayOfVector& subsindices, const ArrayOfVector& values)
{
    // Validate input parameters
    if (values.size() != 1) {
        Error(_W("Right hand values must satisfy left hand side expression."));
    }

    if (subtypes.size() != subsindices.size()) {
        Error(_W("Subtypes and subsindices must have the same size."));
    }

    // Get current class and context
    const std::string currentClass = ClassName(r);
    Context* context = this->getContext();

    // Look up assignment function - try class-specific first, then default
    FunctionDef* funcDef = nullptr;
    const std::string classSpecificFunction
        = getOverloadFunctionName(currentClass, SUBSASGN_OPERATOR_STR);

    if (!context->lookupFunction(classSpecificFunction, funcDef)) {
        const std::string defaultFunction
            = getOverloadFunctionName(NLS_CLASS_ARRAY_STR, SUBSASGN_OPERATOR_STR);
        if (!context->lookupFunction(defaultFunction, funcDef)) {
            Error(_("Function not found:") + " " + classSpecificFunction);
            return {};
        }
    }

    // Validate function type
    if (!(funcDef->type() == NLS_BUILT_IN_FUNCTION || funcDef->type() == NLS_MACRO_FUNCTION)) {
        Error(_W("Type function not valid."));
    }

    // Build function arguments
    ArrayOfVector argIn;
    argIn.reserve(3); // We know we'll have exactly 3 arguments
    argIn.push_back(r);

    // Create substruct for type and subs information
    const stringVector fieldnames = { "type", "subs" };
    const Dimensions dims(1, subtypes.size());
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
    ArrayOf substruct(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

    // Build type and subs vectors more efficiently
    if (!subtypes.empty()) {
        ArrayOfVector typesVector;
        typesVector.reserve(subtypes.size());

        for (const auto& type : subtypes) {
            typesVector.push_back(ArrayOf::characterArrayConstructor(type));
        }

        ArrayOfVector subsVector = subsindices;

        substruct.setFieldAsList("type", typesVector);
        substruct.setFieldAsList("subs", subsVector);
    }

    argIn.push_back(substruct);
    argIn.push_back(values[0]);

    // Execute function with proper call stack management
    const CallStack backupCallStack = callstack;
    try {
        const int nLhs = 1;
        ArrayOfVector rv = funcDef->evaluateFunction(this, argIn, nLhs);

        if (!rv.empty()) {
            rv[0].name(""); // Clear the name of the result
        }

        callstack = backupCallStack;
        return rv;
    } catch (...) {
        // Ensure call stack is restored even on exception
        callstack = backupCallStack;
        throw;
    }
}
//=============================================================================
void
Evaluator::simpleAssign(ArrayOf& r, AbstractSyntaxTreePtr t, ArrayOfVector& value)
{
    auto callStackGuard = [&](auto&& func) {
        callstack.pushID(static_cast<size_t>(t->getContext()));
        try {
            func();
        } catch (...) {
            callstack.popID();
            throw;
        }
        callstack.popID();
    };

    callStackGuard([&]() {
        // Initialize dimensions based on context
        auto calculateRhsDimensions = [&]() -> Dimensions {
            if (!r.isEmpty()) {
                return r.getDimensions();
            } else if (t->opNum != OP_BRACES) {
                return value[0].getDimensions();
            } else {
                Dimensions dims;
                dims.makeScalar();
                return dims;
            }
        };

        Dimensions rhsDimensions = calculateRhsDimensions();

        // Helper lambda to create cell array from expressions
        auto createCellArrayFromExpressions = [&](const ArrayOfVector& expressions) -> ArrayOf {
            if (expressions.empty()) {
                Error(ERROR_INDEX_EXPRESSION_EXPECTED);
            }

            ArrayOf* elements = static_cast<ArrayOf*>(
                ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, expressions.size()));
            ArrayOf cell(NLS_CELL_ARRAY, Dimensions(1, expressions.size()), elements);

            for (size_t k = 0; k < expressions.size(); k++) {
                elements[k] = expressions[k];
            }

            return cell;
        };

        // Helper lambda for class type assignment
        auto handleClassTypeAssignment
            = [&](const std::string& subtype, const ArrayOfVector& expressions) {
                  stringVector subtypes = { subtype };
                  ArrayOfVector subsindices;

                  ArrayOf cell = createCellArrayFromExpressions(expressions);
                  subsindices.push_back(cell);

                  ArrayOfVector res = simpleAssignClass(r, subtypes, subsindices, value);
                  if (res.empty()) {
                      Error(_("Assignment operation failed."));
                  }
                  r = res[0];
              };

        // Handler lambdas for each operation type
        auto handleParensAssignment = [&]() {
            ArrayOfVector expressions = expressionList(t->down, r);

            if (r.isClassType()) {
                handleClassTypeAssignment("()", expressions);
                return;
            }

            if (expressions.empty()) {
                Error(ERROR_INDEX_EXPRESSION_EXPECTED);
            } else if (expressions.size() == 1) {
                r.setVectorSubset(expressions[0], value[0]);
            } else {
                r.setNDimSubset(expressions, value[0]);
            }
        };

        auto handleBracesAssignment = [&]() {
            ArrayOfVector expressions = expressionList(t->down, r);

            if (r.isClassType()) {
                handleClassTypeAssignment("{}", expressions);
                return;
            }

            if (expressions.empty()) {
                Error(ERROR_INDEX_EXPRESSION_EXPECTED);
            } else if (expressions.size() == 1) {
                r.setVectorContentsAsList(expressions[0], value);
            } else {
                r.setNDimContentsAsList(expressions, value);
            }
        };

        auto handleDotAssignment = [&]() {
            const std::string fieldname = t->down->text;

            if (r.isClassType()) {
                stringVector subtypes = { "." };
                ArrayOfVector subsindices;
                subsindices.push_back(ArrayOf::characterArrayConstructor(fieldname));

                ArrayOfVector res = simpleAssignClass(r, subtypes, subsindices, value);
                if (res.size() != 1) {
                    Error(_("Invalid LHS."));
                }
                r = res[0];
                return;
            }

            if (r.isHandle() || r.isGraphicsObject()) {
                setHandle(r, fieldname, value);
            } else if (r.isStruct() || r.isEmpty()) {
                r.setFieldAsList(fieldname, value);
            } else {
                Error(ERROR_ASSIGN_TO_NON_STRUCT);
            }
        };

        auto handleDynamicDotAssignment = [&]() {
            std::string fieldname;
            try {
                ArrayOf fname = expression(t->down);
                fieldname = fname.getContentAsCString();
            } catch (const Exception&) {
                Error(ERROR_DYNAMIC_FIELD_STRING_EXPECTED);
            }

            if (r.isClassType()) {
                stringVector subtypes = { "." };
                ArrayOfVector subsindices;
                subsindices.push_back(ArrayOf::characterArrayConstructor(fieldname));

                ArrayOfVector res = simpleAssignClass(r, subtypes, subsindices, value);
                if (res.size() != 1) {
                    Error(_("Invalid LHS."));
                }
                r = res[0];
            } else if (r.isHandle()) {
                setHandle(r, fieldname, value);
            } else {
                r.setFieldAsList(fieldname, value);
            }
        };

        // Operation dispatch using lambdas in a map-like structure
        static const std::unordered_map<int, std::function<void()>> operationHandlers
            = { { OP_PARENS, [=]() { handleParensAssignment(); } },
                  { OP_BRACES, [=]() { handleBracesAssignment(); } },
                  { OP_DOT, [=]() { handleDotAssignment(); } },
                  { OP_DOTDYN, [=]() { handleDynamicDotAssignment(); } } };

        // Execute the appropriate handler
        auto it = operationHandlers.find(t->opNum);
        if (it != operationHandlers.end()) {
            // Create new lambda to capture current context
            switch (t->opNum) {
            case OP_PARENS:
                handleParensAssignment();
                break;
            case OP_BRACES:
                handleBracesAssignment();
                break;
            case OP_DOT:
                handleDotAssignment();
                break;
            case OP_DOTDYN:
                handleDynamicDotAssignment();
                break;
            default:
                // No operation needed for other cases
                break;
            }
        }
    });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
