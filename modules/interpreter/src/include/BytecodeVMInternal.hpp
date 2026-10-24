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
// Internal declarations shared between the BytecodeVM translation units. The
// helper functions live in the Nelson::bcvm_detail namespace.
//=============================================================================
#include <functional>
#include <string>
#include "ArrayOf.hpp"
#include "BytecodeChunk.hpp"
#include "VMCallFrame.hpp"
//=============================================================================
namespace Nelson {
class Evaluator;
class FunctionDef;
class Context;
//=============================================================================
} // namespace Nelson
//=============================================================================
// classdef property access tokens and error messages shared by the classdef
// helper group.
#define BYTECODE_VM_CLASSDEF_ACCESSOR_GET "get"
#define BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY "Cannot get property: "
#define BYTECODE_VM_CLASSDEF_ERROR_NO_SUCH_PROPERTY "No such property: %s"
#define BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER "Undefined classdef member: "
#define BYTECODE_VM_ERROR_CLASSDEF_MEMBER_CONSTANT_INDEX                                           \
    "BytecodeVM: classdef member constant index out of range."
#define BYTECODE_VM_CLASSDEF_GENERATED_FUNCTION_PREFIX "__classdef_"
#define BYTECODE_VM_CLASSDEF_GENERATED_FUNCTION_SUFFIX "__"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
// Sentinel array names used to tunnel VM-internal signals (no-output, comma
// list, double-row comma list) through ArrayOf values.
inline constexpr const char* NO_OUTPUT_SENTINEL_NAME = "__nelson_bytecode_no_output__";
inline constexpr const char* COMMA_LIST_SENTINEL_NAME = "__nelson_bytecode_comma_list__";
inline constexpr const char* COMMA_LIST_DOUBLE_ROW_SENTINEL_NAME
    = "__nelson_bytecode_comma_list_double_row__";
//=============================================================================
// classdef property access helpers (BytecodeVMClassdef.cpp).
bool
splitClassdefMemberSpec(const std::string& spec, std::string& className, std::string& memberName);
bool
isClassdefStaticMethodName(const std::string& name, const std::string& callerClassName);
void
checkClassdefPropertyGet(Evaluator* eval, const ArrayOf& base, const std::string& field);
ArrayOfVector
getClassdefConstantFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field);
ArrayOfVector
getClassdefDependentFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field);
ArrayOfVector
getClassdefFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field);
bool
tryReadClassdefPlainProperty(
    Evaluator* eval, const ArrayOf& base, const std::string& field, ArrayOf& value);
bool
dispatchRedefinesWholeChain(Evaluator* eval, const ArrayOf& base, const stringVector& subtypes,
    const ArrayOfVector& subsindices, ArrayOf& result);
bool
tryExtractClassDynamicField(Evaluator* eval, const ArrayOf& base, const ArrayOf& fieldValue,
    int nLhs, ArrayOfVector& result);
bool
tryAssignClassDynamicField(
    Evaluator* eval, ArrayOf& base, const ArrayOf& fieldValue, const ArrayOf& value);
ArrayOf
classIndexCell(const ArrayOfVector& indices);
// m(key) on a containers.Map with a char key, without its subsref.
bool
tryFastContainersMapLookup(const ArrayOf& base, const ArrayOfVector& indices, ArrayOf& result);
// table and timetable indexing fast paths (BytecodeVMTable.cpp);
// isColonIndex is defined in BytecodeVM.cpp.
bool
isColonIndex(ArrayOf index);
bool
tryFastTableParenSubset(const ArrayOf& base, const ArrayOfVector& indices, ArrayOf& result);
bool
tryFastTableDotVariable(
    Evaluator* eval, const ArrayOf& base, const std::string& field, ArrayOf& result);
bool
tryFastTableDotVariableIndex(Evaluator* eval, const ArrayOf& base, const std::string& field,
    const ArrayOfVector& params, ArrayOf& result);
bool
tryFastTableDotAssign(
    Evaluator* eval, ArrayOf& base, const std::string& field, const ArrayOf& value);
bool
isTableDotIndexAssignTarget(Evaluator* eval, const ArrayOf& base);
ArrayOf
readTableDynamicDotIndex(
    Evaluator* eval, const ArrayOf& base, const ArrayOf& fieldValue, const ArrayOfVector& indices);
ArrayOf
assignTableDynamicDotIndex(Evaluator* eval, const ArrayOf& base, const ArrayOf& fieldValue,
    const ArrayOfVector& indices, const ArrayOf& value, bool isBraceIndex);
//=============================================================================
// Function-handle creation and call helpers (BytecodeVMHandles.cpp). Cold
// relative to the scalar dispatch loop. isAssignedLocalSlot is defined in
// BytecodeVM.cpp (hot leaf, inlined there); declared here for the handle TU.
bool
isAssignedLocalSlot(const BytecodeChunk& chunk, size_t slot);
bool
isRuntimeAssignedLocalSlot(const VMCallFrame& frame, size_t slot);
ArrayOfVector
callFunctionHandle(Evaluator* eval, const ArrayOf& handle, const ArrayOfVector& args, int nLhs);
stringVector
splitAnonymousArguments(const std::string& payload);
ArrayOf
makeAnonymousHandle(Evaluator* eval, VMCallFrame& frame, const std::string& content,
    const stringVector& arguments, const std::string& definitionText);
ArrayOf
makeNamedHandle(Evaluator* eval, VMCallFrame& frame, const std::string& name);
void
syncNestedFunctionHandlesInFrame(const BytecodeChunk& chunk, VMCallFrame& frame);
//=============================================================================
// Output-probe / silent varargout-retry helpers (BytecodeVMOutputProbe.cpp).
// Cold: only the rare "macro reached varargout{nargout} with no output" retry
// path and the implicit-display detection go through these.
bool
shouldRetrySilentVarargoutCall(FunctionDef* fdef);
ArrayOfVector
evaluateFunctionAndDetectOutput(
    Evaluator* eval, FunctionDef* fdef, const ArrayOfVector& args, int nout, bool& hasOutput);
ArrayOfVector
callFunctionHandleAndDetectOutput(
    Evaluator* eval, const ArrayOf& handle, const ArrayOfVector& args, int nout, bool& hasOutput);
// Displays the result of an implicit call on an object whose class overloads
// functionName with an output; returns false when there is no such overload.
bool
displayImplicitObjectOverloadCall(Evaluator* eval, const std::string& functionName,
    const ArrayOfVector& args, bool refreshCallerContext,
    const std::function<void()>& refreshLocalsFromContext);
//=============================================================================
// Interactive debug prompt helpers (BytecodeVMDebug.cpp). The frame locals
// are published to the context scope before the prompt (syncToContext) and
// reloaded after it (refreshFromContext), so K>> commands see and modify them.
// Both return true when the current chunk must stop (dbquit, abort, quit or
// end of input).
bool
runDebugPrompt(Evaluator* eval, const std::function<void()>& syncToContext,
    const std::function<void()>& refreshFromContext);
bool
handleDebugStatement(Evaluator* eval, VMCallFrame& frame, const Instruction& ins,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext);
bool
handleKeyboardStatement(Evaluator* eval, VMCallFrame& frame, const Instruction& ins,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext);
//=============================================================================
// Raises "Undefined variable: <name>", or "Not enough input arguments." when
// name is a formal input parameter of chunk that the caller did not pass
// (BytecodeVMErrors.cpp).
void
raiseUndefinedVariable(Context* context, const BytecodeChunk& chunk, const std::string& name);
// A{I} = B on a function handle raises "brace indexing is not supported"
// (BytecodeVMErrors.cpp).
void
checkBracesAssignTarget(const ArrayOf& base);
// Called when T.Properties.<name>(indices) or T.Properties.<name>{indices}
// fails: raises the table error when the index goes past the table dimension
// <name> follows, as the table subsref does; returns otherwise. The first form
// finds T and <name> from the read at chunk.code[failingPc]
// (BytecodeVMErrors.cpp).
void
raiseTablePropertiesIndexError(Evaluator* eval, const BytecodeChunk& chunk,
    const VMCallFrame& frame, size_t failingPc, const ArrayOfVector& indices);
void
raiseTablePropertiesIndexError(Evaluator* eval, const ArrayOf& table, const std::string& property,
    const ArrayOfVector& indices);
// Replaces each class object or handle index having a subsindex method by the
// ordinal index it returns (SubsindexOperator.cpp).
void
resolveObjectIndices(Evaluator* eval, ArrayOfVector& indices);
// Brace read of the parent level of a chained assignment (x{2}.a = v): true
// when the element does not exist yet (empty base, or cell element past the
// bounds) and reads as empty so the store-back creates or grows the cell, as
// x{3} = v does; raises "brace indexing is not supported" on a non-empty value
// that is neither a cell nor a string array (BytecodeVMBraceAssignment.cpp).
bool
braceAssignmentParentIsNew(const ArrayOf& base, const ArrayOfVector& indices);
// Raises "dot indexing is not supported" (Nelson:structAssToNonStruct)
// (BytecodeVMDotAssignment.cpp).
[[noreturn]] void
raiseStructAssToNonStruct();
// A.field = B on a value that is not an object nor a handle: raises
// Nelson:structAssToNonStruct when A is neither empty nor a struct; function
// handles and string arrays keep "Cannot apply A.field_name = B to non
// struct-array object A." (BytecodeVMDotAssignment.cpp).
void
checkDotAssignTarget(const ArrayOf& base);
// Parent level of a chained dot assignment (s.a.b = v with s.a numeric,
// x(2).b = v past the end of a double x): raises Nelson:structAssToNonStruct
// on a non-empty value that is neither a struct, a string array, an object nor
// a handle (BytecodeVMDotAssignment.cpp).
void
checkDotAssignParent(const ArrayOf& base);
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
