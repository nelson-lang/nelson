//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <functional>
#include <variant>
#include <memory>
#include <limits>
#include <cmath>
#include <complex>
#include <algorithm>
#include <cctype>
#include <unordered_map>
#include <unordered_set>
#include "AnonymousMacroFunctionDef.hpp"
#include "BuiltInFunctionDef.hpp"
#include "BytecodeVM.hpp"
#include "BytecodeVMFrame.hpp"
#include "BytecodeVMInternal.hpp"
#include "BytecodeVMRange.hpp"
#include "BytecodeVMStack.hpp"
#include "CallbackQueue.hpp"
#include "CheckIfWhileCondition.hpp"
#include "ClassName.hpp"
#include "ClassdefMixinNames.hpp"
#include "ClassToString.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefSuperCall.hpp"
#include "ClassdefConstruct.hpp"
#include "ClassdefPropertyAccess.hpp"
#include "ClassdefWeakReference.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "ClearHandle.hpp"
#include "Context.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "EventQueue.hpp"
#include "NelsonException.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "FunctionDef.hpp"
#include "FunctionsInMemory.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "IsValidVariableName.hpp"
#include "MacroFunctionDef.hpp"
#include "MainThreadTaskQueue.hpp"
#include "MException.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "NelsonPrint.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadDispatchArgument.hpp"
#include "operators/BinaryOperatorHelpers.hpp"
#include "OverloadName.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "RuntimeDispatch.hpp"
#include "Scope.hpp"
#include "ScalarBuiltinFastPath.hpp"
#include "VMCallFrame.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Helper functions shared across the BytecodeVM translation units. Shared
// constants and error-message macros live in BytecodeVMInternal.hpp.
namespace bcvm_detail {
    //=============================================================================
    // A callee may have synced locals into the scope pushed by its caller. Drop them first so
    // every local dies when the frame releases its slot, in declaration order. Nested functions
    // keep the scope for their variable write-back.
    // A handle object, or a function handle (whose definition owns the
    // workspace it captured): values whose release has visible effects.
    inline bool
    valueOwnsObject(const ArrayOf& value)
    {
        const NelsonType dataClass = value.dataClass();
        return dataClass == NLS_HANDLE || dataClass == NLS_FUNCTION_HANDLE;
    }
    //=============================================================================
    void
    dropCallerScopeVariablesBeforeFrame(
        Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame)
    {
        if (!frame.hasContextScope || frame.lazyScopePushed || chunk.selfIsNestedMacro
            || chunk.isScript || chunk.isAnonymous || eval->getContext() == nullptr) {
            return;
        }
        Scope* scope = eval->getContext()->getCurrentScope();
        if (scope != nullptr) {
            scope->clearVariables();
        }
    }
    //=============================================================================
    ArrayOf
    noOutputSentinel()
    {
        ArrayOf value = ArrayOf::empty();
        value.name(NO_OUTPUT_SENTINEL_NAME);
        return value;
    }
    //=============================================================================
    ArrayOf
    logicalScalarFromKeyword(bool value, const ArrayOfVector& args)
    {
        Dimensions dim;
        indexType idxMax = static_cast<indexType>(args.size());
        if (args.empty()) {
            dim = Dimensions(1, 1);
        } else {
            if (args.size() >= 2) {
                indexType pos = static_cast<indexType>(args.size() - 2);
                if (args[pos].isRowVectorCharacterArray() || args[pos].isScalarStringArray()) {
                    std::wstring arg = args[pos].asWideString();
                    if (arg != L"like") {
                        Error(L"Nelson:interpreter:invalidValue", L"Wrong value for argument #%d.",
                            pos + 1);
                    }
                    ArrayOf likeValue = args[pos + 1];
                    if (likeValue.dataClass() != NLS_LOGICAL) {
                        Error(L"Nelson:interpreter:invalidLikeInput",
                            L"Input following 'like' is not a logical array.");
                    }
                    idxMax = pos;
                    if (idxMax == 0) {
                        dim[0] = 1;
                        dim[1] = 1;
                    }
                }
            }
            for (indexType k = 0; k < idxMax; ++k) {
                dim[k] = args[k].asScalarIndex();
            }
            if (idxMax == 1) {
                dim[1] = dim[0];
            }
            dim.simplify();
        }

        logical* data = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.elementCount(), stringVector(), true));
        std::memset(data, value ? 1 : 0, sizeof(logical) * dim.elementCount());
        return ArrayOf(NLS_LOGICAL, dim, data, false);
    }
    //=============================================================================
    void
    declareGlobalVariable(Context* context, const std::string& name)
    {
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:interpreter:invalidVariableName", L"Valid variable name expected.");
        }
        if (context->isLockedVariable(name)) {
            Error(L"Nelson:interpreter:variableLocked", L"variable is locked: %s",
                utf8_to_wstring(name));
        }
        context->addGlobalVariable(name);
    }
    //=============================================================================
    void
    declarePersistentVariable(Context* context, const std::string& name, bool isScriptChunk)
    {
        if (isScriptChunk || context->getCurrentScope()->getName() == "base") {
            Error(L"Nelson:interpreter:persistentNotInFunction",
                L"A 'persistent' declaration is only allowed in a script file function.");
        }
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:interpreter:invalidVariableName", L"Valid variable name expected.");
        }
        if (context->isLockedVariable(name)) {
            Error(L"Nelson:interpreter:variableLocked", L"variable is locked: %s",
                utf8_to_wstring(name));
        }
        context->addPersistentVariable(name);
    }
    //=============================================================================
    bool
    isNoOutputSentinel(const ArrayOf& value)
    {
        return value.name() == NO_OUTPUT_SENTINEL_NAME;
    }
    //=============================================================================
    bool
    tryLoadImplicitArgumentCount(Context* context, const std::string& name, ArrayOf& value)
    {
        if (context == nullptr || context->getCurrentScope() == nullptr
            || context->getCurrentScope()->getName() == "base") {
            return false;
        }
        if (name == "nargin") {
            value = ArrayOf::doubleScalar(context->getCurrentScope()->getNargIn());
            return true;
        }
        if (name == "nargout") {
            value = ArrayOf::doubleScalar(context->getCurrentScope()->getNargOut());
            return true;
        }
        return false;
    }
    //=============================================================================
    ArrayOf
    // callerPushedScope: see BytecodeVM::executeFunction; no default so that
    // every direct call site states whether it pushed a scope for the callee.
    executeFunctionOneArgOneOutput(
        Evaluator* eval, const BytecodeChunk& chunk, const ArrayOf& argIn, bool callerPushedScope);
    bool
    canDirectSelfBytecodeCall(Evaluator* eval, const BytecodeChunk& callerChunk,
        FunctionDef* funcDef, const ArrayOf* args, size_t nargs, uint16_t nout,
        BytecodeChunk*& calleeChunk);
    bool
    canDirectLeafMacroBytecodeCall(Evaluator* eval, FunctionDef* funcDef, const ArrayOfVector& args,
        uint16_t nout, BytecodeChunk*& calleeChunk);
    //=============================================================================
    bool
    isResolvedBuiltinStringFunction(const std::string& name, FunctionDef* fdef)
    {
        return name == "string" && fdef != nullptr && fdef->type() == NLS_BUILT_IN_FUNCTION
            && fdef->getName() == "string";
    }
    //=============================================================================
    // mirrors the isa builtin (modules/types isaBuiltin.cpp): keep both in sync
    bool
    nativeIsaCheck(const ArrayOf& value, const std::string& classnameExpected)
    {
        if (classnameExpected == "numeric") {
            return value.isNumeric();
        }
        if (classnameExpected == "float") {
            return value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_DCOMPLEX
                || value.dataClass() == NLS_SINGLE || value.dataClass() == NLS_SCOMPLEX;
        }
        if (classnameExpected == "integer") {
            return IS_INTEGER_TYPE(value.dataClass());
        }
        if (value.dataClass() == NLS_HANDLE) {
            return classnameExpected == NLS_HANDLE_STR
                || classdefIsA(value.handleClassName(), classnameExpected);
        }
        if (value.dataClass() == NLS_GO_HANDLE) {
            const std::string currentClassName = ClassName(value);
            return classnameExpected == NLS_HANDLE_STR || classnameExpected == NLS_GO_HANDLE_STR
                || classnameExpected == NLS_GRAPHICS_OBJECT_CLASS_STR
                || currentClassName == classnameExpected;
        }
        if (value.dataClass() == NLS_CLASS_ARRAY || value.dataClass() == NLS_CLASSDEF_ARRAY) {
            return classnameExpected == NLS_CLASS_ARRAY_STR
                || classdefIsA(value.classType(), classnameExpected);
        }
        const std::string currentClassName
            = value.isSparse() ? ClassToString(value.dataClass()) : ClassName(value);
        return currentClassName == classnameExpected;
    }
    //=============================================================================
    // JUMP_IF_ARG_PRED only short-circuits while its predicate name still
    // resolves to the pristine builtin; the verdict is memoized against the
    // function-tables generation.
    bool
    fusedArgPredBuiltinIsCanonical(Evaluator* eval, uint8_t predId)
    {
        static uint64 cacheGeneration = ~static_cast<uint64>(0);
        static uint8_t verdicts[2] = { 0, 0 };
        if (predId > 1) {
            return false;
        }
        const uint64 generation = FunctionsInMemory::getInstance()->generation();
        if (cacheGeneration != generation) {
            verdicts[0] = 0;
            verdicts[1] = 0;
            cacheGeneration = generation;
        }
        if (verdicts[predId] == 0) {
            const char* name = predId == ARG_PRED_ISA ? "isa" : "isscalar";
            FunctionDef* fdef = nullptr;
            const bool canonical = eval->lookupFunction(name, fdef) && fdef != nullptr
                && fdef->type() == NLS_BUILT_IN_FUNCTION && fdef->getName() == name;
            verdicts[predId] = canonical ? 1 : 2;
        }
        return verdicts[predId] == 1;
    }
    //=============================================================================
    bool
    tryEvaluateBuiltinStringFast(const ArrayOf& arg, ArrayOf& result)
    {
        if (!arg.isStringArray() && !arg.isRowVectorCharacterArray()) {
            return false;
        }
        bool needToOverload = false;
        result = ArrayOf::toStringArray(arg, needToOverload);
        return !needToOverload;
    }
    //=============================================================================
    ArrayOf
    commaListSentinel(const ArrayOfVector& values)
    {
        ArrayOfMatrix matrix;
        matrix.push_back(values);
        ArrayOf value = ArrayOf::cellArray(matrix);
        value.name(COMMA_LIST_SENTINEL_NAME);
        return value;
    }
    //=============================================================================
    ArrayOfVector
    getOrInvokeHandle(
        Evaluator* eval, const ArrayOf& base, const std::string& field, const ArrayOfVector& params)
    {
        if (base.isGraphicsObject()) {
            return eval->bytecodeGetHandle(base, field, params);
        }
        return eval->bytecodeGetOrInvokeHandle(base, field, params);
    }
    //=============================================================================
    ArrayOf
    classIndexCell(const ArrayOfVector& indices)
    {
        ArrayOfMatrix matrix;
        matrix.push_back(indices);
        return ArrayOf::cellArray(matrix);
    }
    //=============================================================================
    // x.(indexOp): a nelson.indexing.IndexingOperation (scalar or array) used as a dynamic field
    // name is applied as a subsref (read) / subsasgn (write) chain, as in obj.Data.(indexOp). See
    // applyIndexingOperation* below.
    bool
    isIndexingOperationValue(const ArrayOf& value)
    {
        return value.isClassType() && value.classType() == NLS_INDEXING_OPERATION;
    }
    //=============================================================================
    bool
    isCommaListSentinel(const ArrayOf& value)
    {
        return value.name() == COMMA_LIST_SENTINEL_NAME
            || value.name() == COMMA_LIST_DOUBLE_ROW_SENTINEL_NAME;
    }
    //=============================================================================
    bool
    isCommaListDoubleRowSentinel(const ArrayOf& value)
    {
        return value.name() == COMMA_LIST_DOUBLE_ROW_SENTINEL_NAME;
    }
    //=============================================================================
    ArrayOfVector
    commaListValues(ArrayOf value)
    {
        if (isCommaListDoubleRowSentinel(value)) {
            ArrayOfVector values;
            const indexType elementCount = value.elementCount();
            values.reserve(static_cast<size_t>(elementCount));
            const auto* data = static_cast<const double*>(value.dataPointer());
            for (indexType k = 0; k < elementCount; ++k) {
                values.push_back(ArrayOf::doubleScalar(data[k]));
            }
            return values;
        }
        ArrayOf index = ArrayOf::characterArray(":");
        ArrayOfVector values = value.vectorContentsAsList(index);
        // elements may be named arguments a builtin stored: they carry no input name
        for (auto& element : values) {
            if (!element.name().empty()) {
                element.name("");
            }
        }
        return values;
    }
    //=============================================================================
    ArrayOfVector
    expandCommaLists(const ArrayOfVector& args)
    {
        ArrayOfVector expanded;
        expanded.reserve(args.size());
        for (const auto& arg : args) {
            if (isCommaListSentinel(arg)) {
                expanded += commaListValues(arg);
            } else {
                expanded.push_back(arg);
            }
        }
        return expanded;
    }
    //=============================================================================
    bool
    tryCommaListScalarDoubleRow(const ArrayOfVector& args, ArrayOf& result)
    {
        if (args.size() != 1 || !isCommaListSentinel(args[0])) {
            return false;
        }
        const ArrayOf& list = args[0];
        if (isCommaListDoubleRowSentinel(list)) {
            result = list;
            result.name("");
            return true;
        }
        if (!list.isCell() || list.isSparse()) {
            return false;
        }
        const indexType elementCount = list.elementCount();
        std::unique_ptr<double[]> data;
        if (elementCount > 0) {
            try {
                data = std::make_unique<double[]>(static_cast<size_t>(elementCount));
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            const auto* elements = static_cast<const ArrayOf*>(list.dataPointer());
            for (indexType k = 0; k < elementCount; ++k) {
                const ArrayOf& value = elements[k];
                if (!value.isDoubleType(true) || value.elementCount() != 1) {
                    return false;
                }
                data[static_cast<size_t>(k)] = *static_cast<const double*>(value.dataPointer());
            }
        }
        Dimensions dimensions(1, elementCount);
        result = ArrayOf(NLS_DOUBLE, dimensions, data.get());
        if (data) {
            data.release();
        }
        return true;
    }
    //=============================================================================
    bool
    isColonIndex(ArrayOf index)
    {
        if ((index.dataClass() == NLS_CHAR && index.elementCount() == 1)
            || (index.isStringArray() && index.isScalar())) {
            try {
                return index.asWideString() == L":";
            } catch (const NelsonException&) {
                return false;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    isIdentifierPart(char ch)
    {
        return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
    }
    //=============================================================================
    std::string
    sanitizeGeneratedClassdefFunctionName(std::string value)
    {
        for (char& ch : value) {
            if (!isIdentifierPart(ch)) {
                ch = '_';
            }
        }
        return value;
    }
    //=============================================================================
    std::string
    generatedClassdefConstructorFunctionName(const std::string& className)
    {
        return BYTECODE_VM_CLASSDEF_GENERATED_FUNCTION_PREFIX
            + sanitizeGeneratedClassdefFunctionName(className) + "_constructor"
            + BYTECODE_VM_CLASSDEF_GENERATED_FUNCTION_SUFFIX;
    }
    //=============================================================================
    // Condition of if/while or operand of && / ||. A class or handle value dispatches to its
    // `logical` overload first (`if obj` works when the class has one) and errors otherwise.
    bool
    conditionAsBoolean(Evaluator* eval, const ArrayOf& value)
    {
        if (value.isClassType() || value.isHandle()) {
            bool wasFound = false;
            ArrayOf converted = callOperatorOverload(eval, value, "logical", wasFound);
            if (wasFound) {
                return checkIfWhileCondition(converted);
            }
            Error("Nelson:interpreter:cannotConvertToLogical",
                "Conversion to logical from %s is not possible.", ClassName(value));
        }
        return checkIfWhileCondition(value);
    }
    //=============================================================================
    bool
    checkShortcutScalarCondition(Evaluator* eval, const ArrayOf& value, bool isAnd)
    {
        if (!value.isScalar()) {
            RaiseError(isAnd
                    ? resolveMessageTemplate(L"Nelson:interpreter:andOperandNotLogical",
                          L"Operand to && operator must be convertible to logical scalar values.")
                    : resolveMessageTemplate(L"Nelson:interpreter:orOperandNotLogical",
                          L"Operand to || operator must be convertible to logical scalar values."),
                L"Nelson:interpreter:invalidLogicalOperand");
        }
        return conditionAsBoolean(eval, value);
    }
    //=============================================================================
    void
    expandColonContentAssignmentIndices(const ArrayOf& base, ArrayOfVector& indices)
    {
        if (indices.size() == 1) {
            if (isColonIndex(indices[0])) {
                indices[0] = ArrayOf::integerRange(1, 1, base.elementCount(), false);
            }
            return;
        }
        indexType L = static_cast<indexType>(indices.size());
        indexType nd = base.dimensions().length();
        for (size_t k = 0; k < indices.size(); ++k) {
            if (isColonIndex(indices[k])) {
                indexType len;
                if (static_cast<indexType>(k) == L - 1 && L < nd) {
                    // Fewer subscripts than dimensions: the last colon spans the
                    // product of the trailing dimensions (column-major fold).
                    len = 1;
                    for (indexType d = k; d < nd; ++d) {
                        len *= base.dimensionLength(static_cast<int>(d));
                    }
                } else {
                    len = base.dimensionLength(static_cast<int>(k));
                }
                indices[k] = ArrayOf::integerRange(1, 1, len, false);
            }
        }
    }
    //=============================================================================
    void
    expandColonParensAssignmentIndices(
        const ArrayOf& base, ArrayOfVector& indices, const ArrayOf& value)
    {
        if (value.isEmpty()) {
            // A(...) = [] deletes: the subset functions need the literal ':'
            // subscripts to tell them apart from an explicit full range.
            return;
        }
        if (base.isEmpty(true) && indices.size() > 1) {
            // only a 0x0 base takes the extents of its ':' subscripts from the
            // value; a partially empty base (2x0) keeps its own extents
            size_t colonCount = 0;
            for (const ArrayOf& index : indices) {
                if (isColonIndex(index)) {
                    ++colonCount;
                }
            }
            for (size_t k = 0; k < indices.size(); ++k) {
                if (isColonIndex(indices[k])) {
                    indexType len = value.dimensionLength(static_cast<int>(k));
                    if (colonCount == 1 && value.isVector()) {
                        indexType fixedCount = 1;
                        for (size_t j = 0; j < indices.size(); ++j) {
                            if (j != k) {
                                fixedCount *= indices[j].elementCount();
                            }
                        }
                        indexType valueCount = value.elementCount();
                        if (fixedCount > 0 && valueCount % fixedCount == 0) {
                            len = valueCount / fixedCount;
                        } else if (len == 0) {
                            len = valueCount;
                        }
                    }
                    indices[k] = ArrayOf::integerRange(1, 1, len, false);
                }
            }
            return;
        }
        expandColonContentAssignmentIndices(base, indices);
    }
    //=============================================================================
    void
    normalizeParensAssignmentValueForTarget(const ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() < 2 || value.elementCount() == 0 || !value.isVector()) {
            return;
        }
        Dimensions targetDims;
        indexType targetCount = 1;
        for (size_t k = 0; k < indices.size(); ++k) {
            indexType len = indices[k].elementCount();
            targetDims[static_cast<indexType>(k)] = len;
            targetCount *= len;
        }
        if (targetCount == value.elementCount() && !value.dimensions().equals(targetDims)) {
            value.reshape(targetDims);
        }
    }
    //=============================================================================
    indexType
    contentAssignmentTargetCount(const ArrayOfVector& indices)
    {
        if (indices.empty()) {
            return 0;
        }
        indexType count = 1;
        for (const ArrayOf& index : indices) {
            count *= index.elementCount();
        }
        return count;
    }
    //=============================================================================
    uint16_t
    outputCountFromSpec(Evaluator* eval, const ArrayOf& spec)
    {
        indexType count = spec.elementCount();
        if (spec.isClassType() || spec.isHandle()) {
            // [c{obj}] = f(...) fills one target per subsindex position of obj
            ArrayOfVector index { spec };
            resolveObjectIndices(eval, index);
            count = index[0].elementCount();
        }
        if (count > std::numeric_limits<uint16_t>::max()) {
            Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
        }
        return static_cast<uint16_t>(count);
    }
    //=============================================================================
    void
    normalizeParensAssignmentValueForColon(ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() != 2 || !value.isColumnVector() || !isColonIndex(indices[1])) {
            return;
        }
        if (!indices[0].isScalar() || value.elementCount() == 0) {
            return;
        }
        Dimensions dims(1, value.elementCount());
        value.reshape(dims);
    }
    //=============================================================================
    bool
    scalarOrdinalIndex(const ArrayOf& index, indexType& value);
    //=============================================================================
    bool
    scalarDoubleOrdinalIndex(const ArrayOf& index, indexType& value)
    {
        if (!index.isDoubleType(true) || index.elementCount() != 1) {
            return false;
        }
        double raw = *static_cast<const double*>(index.dataPointer());
        double rounded = std::floor(raw);
        if (raw != rounded || !std::isfinite(raw) || raw < 1
            || raw >= std::ldexp(1.0, std::numeric_limits<indexType>::digits)) {
            return false;
        }
        value = static_cast<indexType>(raw);
        return true;
    }
    //=============================================================================
    bool
    doubleOrdinalVectorIndex(
        const ArrayOf& index, indexType limit, std::vector<indexType>& ordinals)
    {
        if (!index.isDoubleType(true) || index.isSparse()) {
            return false;
        }
        const indexType count = index.elementCount();
        if (count == 0) {
            return false;
        }
        const auto* values = static_cast<const double*>(index.dataPointer());
        ordinals.resize(count);
        for (indexType k = 0; k < count; ++k) {
            const double raw = values[k];
            const double rounded = std::floor(raw);
            if (raw != rounded || !std::isfinite(raw) || raw < 1
                || raw > static_cast<double>(limit)) {
                return false;
            }
            ordinals[k] = static_cast<indexType>(raw);
        }
        return true;
    }
    //=============================================================================
    bool
    tryDoubleMatrix2DSubsetDirect(
        const ArrayOf& base, const ArrayOf& rowIndex, const ArrayOf& colIndex, ArrayOf& result)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || base.isComplex()) {
            return false;
        }
        const indexType rows = base.dimensionLength(0);
        const indexType cols = base.dimensionLength(1);
        std::vector<indexType> rowOrdinals;
        std::vector<indexType> colOrdinals;
        if (!doubleOrdinalVectorIndex(rowIndex, rows, rowOrdinals)
            || !doubleOrdinalVectorIndex(colIndex, cols, colOrdinals)) {
            return false;
        }
        const indexType outRows = static_cast<indexType>(rowOrdinals.size());
        const indexType outCols = static_cast<indexType>(colOrdinals.size());
        auto* output = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, outRows * outCols, stringVector(), false));
        const auto* source = static_cast<const double*>(base.dataPointer());
        for (indexType c = 0; c < outCols; ++c) {
            const indexType sourceColumnOffset = (colOrdinals[c] - 1) * rows;
            const indexType outputColumnOffset = c * outRows;
            for (indexType r = 0; r < outRows; ++r) {
                output[outputColumnOffset + r] = source[sourceColumnOffset + rowOrdinals[r] - 1];
            }
        }
        result = ArrayOf(NLS_DOUBLE, Dimensions(outRows, outCols), output);
        return true;
    }
    //=============================================================================
    bool
    tryDoubleLinearSubsetDirect(const ArrayOf& base, const ArrayOf& index, ArrayOf& result)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || base.isComplex()) {
            return false;
        }
        std::vector<indexType> ordinals;
        if (!doubleOrdinalVectorIndex(index, base.elementCount(), ordinals)) {
            return false;
        }
        Dimensions outputDims(index.dimensions());
        if (!base.isScalar() && base.isVector() && index.isVector()) {
            outputDims = base.isRowVector() ? Dimensions(1, index.elementCount())
                                            : Dimensions(index.elementCount(), 1);
        }
        outputDims.simplify();
        auto* output = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, index.elementCount(), stringVector(), false));
        const auto* source = static_cast<const double*>(base.dataPointer());
        const indexType count = static_cast<indexType>(ordinals.size());
        for (indexType k = 0; k < count; ++k) {
            output[k] = source[ordinals[k] - 1];
        }
        result = ArrayOf(NLS_DOUBLE, outputDims, output);
        return true;
    }
    //=============================================================================
    bool
    tryDoubleLogicalMaskSubsetDirect(const ArrayOf& base, const ArrayOf& mask, ArrayOf& result)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || base.isComplex()
            || mask.dataClass() != NLS_LOGICAL || mask.isSparse()) {
            return false;
        }
        const auto* maskData = static_cast<const logical*>(mask.dataPointer());
        const indexType maskCount = mask.elementCount();
        const indexType bound = base.elementCount();
        indexType selectedCount = 0;
        indexType lastSelected = 0;
        for (indexType k = 0; k < maskCount; ++k) {
            if (maskData[k] != 0) {
                ++selectedCount;
                lastSelected = k;
            }
        }
        if (selectedCount != 0 && lastSelected >= bound) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        Dimensions outputDims;
        if (mask.isScalar()) {
            // A scalar logical index yields a 0x0 empty when false, as in ArrayOf::vectorSubset.
            outputDims = (selectedCount == 0) ? Dimensions(0, 0) : Dimensions(1, 1);
        } else {
            outputDims
                = mask.isRowVector() ? Dimensions(1, selectedCount) : Dimensions(selectedCount, 1);
            if (!base.isScalar() && base.isVector() && outputDims.isVector()) {
                outputDims = base.isRowVector() ? Dimensions(1, selectedCount)
                                                : Dimensions(selectedCount, 1);
            }
        }
        outputDims.simplify();
        auto* output = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, selectedCount, stringVector(), false));
        const auto* source = static_cast<const double*>(base.dataPointer());
        indexType out = 0;
        for (indexType k = 0; k < maskCount; ++k) {
            if (maskData[k] != 0) {
                output[out++] = source[k];
            }
        }
        result = ArrayOf(NLS_DOUBLE, outputDims, output);
        return true;
    }
    //=============================================================================
    bool
    tryAssignStringScalarAllElements(ArrayOf& base, const ArrayOf& value);
    //=============================================================================
    bool
    tryAssignRowColonSlice(ArrayOf& base, ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() != 2 || !isColonIndex(indices[1]) || !indices[0].isScalar()
            || isColonIndex(indices[0]) || value.isEmpty()) {
            return false;
        }
        indexType nRows = base.dimensionLength(0);
        indexType nCols = base.dimensionLength(1);
        if (value.elementCount() != nCols) {
            return false;
        }
        indexType row = static_cast<indexType>(indices[0].asInt64Scalar());
        ArrayOf linear = ArrayOf::integerRange(row, nRows, row + (nCols - 1) * nRows, false);
        base.setVectorSubset(linear, value);
        return true;
    }
    //=============================================================================
    bool
    tryAssignCellColonFromScalarCell(
        ArrayOf& base, const ArrayOfVector& indices, const ArrayOf& value);
    bool
    tryAssignCellColonFromScalarCell(ArrayOf& base, const ArrayOf& value);
    bool
    replaceArrayOfElementsWithScalar(ArrayOf& base, NelsonType type, const ArrayOf& element);
    //=============================================================================
    bool
    tryAssignVectorColon(ArrayOf& base, ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() != 1 || !isColonIndex(indices[0]) || value.isEmpty()) {
            // A(:) = [] is a deletion: the subset function keeps the class
            return false;
        }
        if (tryAssignStringScalarAllElements(base, value)) {
            return true;
        }
        if (tryAssignCellColonFromScalarCell(base, indices, value)) {
            return true;
        }
        if (value.dataClass() == base.dataClass() && value.elementCount() == base.elementCount()
            && value.dimensions().equals(base.dimensions())) {
            base.setValue(value);
            return true;
        }
        ArrayOf linear = ArrayOf::integerRange(1, 1, base.elementCount(), false);
        base.setVectorSubset(linear, value);
        return true;
    }
    //=============================================================================
    bool
    tryAssignCellColonFromScalarCell(
        ArrayOf& base, const ArrayOfVector& indices, const ArrayOf& value)
    {
        if (!base.isCell() || base.isSparse() || indices.size() != 1 || !isColonIndex(indices[0])
            || !value.isCell() || value.elementCount() != 1) {
            return false;
        }
        return tryAssignCellColonFromScalarCell(base, value);
    }
    //=============================================================================
    bool
    tryAssignCellColonFromScalarCell(ArrayOf& base, const ArrayOf& value)
    {
        if (!base.isCell() || base.isSparse() || !value.isCell() || value.elementCount() != 1) {
            return false;
        }
        const auto* rhs = static_cast<const ArrayOf*>(value.dataPointer());
        ArrayOf fill = rhs[0];
        return replaceArrayOfElementsWithScalar(base, NLS_CELL_ARRAY, fill);
    }
    //=============================================================================
    bool
    replaceArrayOfElementsWithScalar(ArrayOf& base, NelsonType type, const ArrayOf& element)
    {
        const indexType elementCount = base.elementCount();
        ArrayOf* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(type, elementCount, stringVector(), false));
        std::fill_n(elements, elementCount, element);
        ArrayOf replacement(type, Dimensions(base.dimensions()), elements);
        base.setValue(replacement);
        return true;
    }
    //=============================================================================
    bool
    tryAssignEmptyCellColumn(ArrayOf& base, const ArrayOfVector& indices, ArrayOf& value)
    {
        if (!base.isEmpty() || !value.isCell() || indices.size() != 2 || !isColonIndex(indices[0])
            || !indices[1].isScalar()) {
            return false;
        }
        try {
            if (indices[1].asInt64Scalar() != 1) {
                return false;
            }
        } catch (const NelsonException&) {
            return false;
        }
        base.setValue(value);
        return true;
    }
    //=============================================================================
    bool
    tryReadCellScalarContentDirect(const ArrayOf& base, const ArrayOf& index, ArrayOf& value)
    {
        if (!base.isCell() || base.isSparse()) {
            return false;
        }
        indexType ordinal = 0;
        if (!scalarDoubleOrdinalIndex(index, ordinal) || ordinal > base.elementCount()) {
            return false;
        }
        const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
        value = elements[ordinal - 1];
        return true;
    }
    //=============================================================================
    bool
    tryReadCellScalarContentDirect(
        const ArrayOf& base, const ArrayOfVector& indices, ArrayOf& value)
    {
        if (!base.isCell() || base.isSparse() || indices.empty() || indices.size() > 2) {
            return false;
        }
        if (indices.size() == 1) {
            indexType ordinal = 0;
            if (!scalarOrdinalIndex(indices[0], ordinal) || ordinal > base.elementCount()) {
                return false;
            }
            const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
            value = elements[ordinal - 1];
            return true;
        }
        indexType row = 0;
        indexType column = 0;
        if (!scalarOrdinalIndex(indices[0], row) || !scalarOrdinalIndex(indices[1], column)
            || row > base.dimensionLength(0) || column > base.dimensionLength(1)) {
            return false;
        }
        indexType offset = (column - 1) * base.dimensionLength(0) + row - 1;
        const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
        value = elements[offset];
        return true;
    }
    //=============================================================================
    bool
    tryAssignCellScalarContentDirect(ArrayOf& base, const ArrayOf& index, const ArrayOf& value)
    {
        if (!base.isCell() || base.isSparse()) {
            return false;
        }
        indexType ordinal = 0;
        if (!scalarDoubleOrdinalIndex(index, ordinal) || ordinal > base.elementCount()) {
            return false;
        }
        auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        elements[ordinal - 1] = value;
        return true;
    }
    //=============================================================================
    bool
    tryAddCellScalarDoubleContentToLocal(ArrayOf& target, const ArrayOf& base, const ArrayOf& index)
    {
        if (!target.isDoubleType(true) || target.elementCount() != 1 || !base.isCell()
            || base.isSparse()) {
            return false;
        }
        indexType ordinal = 0;
        if (!scalarDoubleOrdinalIndex(index, ordinal) || ordinal > base.elementCount()) {
            return false;
        }
        const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
        const ArrayOf& value = elements[ordinal - 1];
        if (!value.isDoubleType(true) || value.elementCount() != 1) {
            return false;
        }
        *static_cast<double*>(target.readWriteDataPointer())
            += *static_cast<const double*>(value.dataPointer());
        return true;
    }
    //=============================================================================
    bool
    tryAssignCellScalarContentDirect(
        ArrayOf& base, const ArrayOfVector& indices, const ArrayOf& value)
    {
        if (!base.isCell() || base.isSparse() || indices.empty() || indices.size() > 2) {
            return false;
        }
        if (indices.size() == 1) {
            indexType ordinal = 0;
            if (!scalarOrdinalIndex(indices[0], ordinal) || ordinal > base.elementCount()) {
                return false;
            }
            auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
            elements[ordinal - 1] = value;
            return true;
        }
        indexType row = 0;
        indexType column = 0;
        if (!scalarOrdinalIndex(indices[0], row) || !scalarOrdinalIndex(indices[1], column)
            || row > base.dimensionLength(0) || column > base.dimensionLength(1)) {
            return false;
        }
        indexType offset = (column - 1) * base.dimensionLength(0) + row - 1;
        auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        elements[offset] = value;
        return true;
    }
    //=============================================================================
    ArrayOf
    evaluateDefaultClassdefObject(Evaluator* eval, const std::string& className);
    //=============================================================================
    ArrayOf
    buildClassdefArrayFromScalar(ArrayOf scalar, const Dimensions& dimensions)
    {
        if (!scalar.isClassdefType() || !scalar.isScalar()) {
            Error(
                L"Nelson:interpreter:scalarClassdefExpected", L"Scalar classdef object expected.");
        }
        const stringVector fields = scalar.fieldNames();
        void* elements = ArrayOf::allocateArrayOf(
            NLS_CLASSDEF_ARRAY, dimensions.elementCount(), fields, false);
        for (indexType k = 0; k < dimensions.elementCount(); ++k) {
            scalar.copyElements(0, elements, k, 1);
        }
        ArrayOf result(NLS_CLASSDEF_ARRAY, dimensions, elements, false, fields);
        result.setClassType(scalar.classType());
        result.setClassdefLayout(scalar.classdefLayout());
        return result;
    }
    //=============================================================================
    ArrayOf
    buildHandleArrayFromDefaults(Evaluator* eval, const std::string& className,
        const Dimensions& dimensions, const ArrayOf& firstDefault)
    {
        if (!firstDefault.isHandle() || !firstDefault.isScalar()) {
            Error(L"Nelson:interpreter:scalarClassdefHandleExpected",
                L"Scalar classdef handle expected.");
        }
        void* elements = ArrayOf::allocateArrayOf(NLS_HANDLE, dimensions.elementCount(), {}, false);
        for (indexType k = 0; k < dimensions.elementCount(); ++k) {
            ArrayOf object = k == 0 ? firstDefault : evaluateDefaultClassdefObject(eval, className);
            if (!object.isHandle() || !object.isScalar() || ClassName(object) != className) {
                Error(L"Nelson:interpreter:cannotCreateClassdefObject",
                    L"Cannot create default classdef object.");
            }
            object.copyElements(0, elements, k, 1);
        }
        return ArrayOf(NLS_HANDLE, dimensions, elements, false);
    }
    //=============================================================================
    void
    copyClassdefBaseIntoExpandedArray(ArrayOf base, ArrayOf& expanded)
    {
        if (base.isEmpty()) {
            return;
        }
        void* expandedData = expanded.readWriteDataPointer();
        Dimensions currentPosition(base.dimensions().length());
        indexType rowCount = base.dimensions()[0];
        indexType sourceIndex = 0;
        while (currentPosition.inside(base.dimensions())) {
            indexType destinationIndex = expanded.dimensions().mapPoint(currentPosition);
            base.copyElements(sourceIndex, expandedData, destinationIndex, rowCount);
            currentPosition.incrementModulo(base.dimensions(), 1);
            sourceIndex += rowCount;
        }
    }
    //=============================================================================
    ArrayOf
    evaluateDefaultClassdefObject(Evaluator* eval, const std::string& className)
    {
        if (eval == nullptr || eval->getContext() == nullptr) {
            Error(L"Nelson:interpreter:cannotCreateClassdefObject",
                L"Cannot create default classdef object.");
        }
        // value-class defaults are immutable copies, so the constructor result
        // can be reused until a class definition changes; handle classes must
        // keep constructing fresh objects (each default has its own identity)
        const bool isHandleClass = classdefIsHandleClass(className);
        static std::unordered_map<std::string, ArrayOf> valueDefaultCache;
        static size_t cacheGeneration = 0;
        if (!isHandleClass) {
            if (cacheGeneration != classdefRegistryGeneration()) {
                valueDefaultCache.clear();
                cacheGeneration = classdefRegistryGeneration();
            }
            auto cached = valueDefaultCache.find(className);
            if (cached != valueDefaultCache.end()) {
                return cached->second;
            }
        }
        FunctionDef* constructor = nullptr;
        if (!eval->getContext()->lookupFunction(
                generatedClassdefConstructorFunctionName(className), constructor)
            && !eval->getContext()->lookupFunction(className, constructor)) {
            constructor = nullptr;
        }
        if (constructor == nullptr) {
            Error(L"Nelson:interpreter:cannotCreateClassdefObject",
                L"Cannot create default classdef object.");
        }
        ArrayOfVector result = constructor->evaluateFunction(eval, {}, 1);
        if (result.empty() || !result[0].isScalar()) {
            Error(L"Nelson:interpreter:cannotCreateClassdefObject",
                L"Cannot create default classdef object.");
        }
        if (isHandleClass) {
            if (!result[0].isHandle() || ClassName(result[0]) != className) {
                Error(L"Nelson:interpreter:cannotCreateClassdefObject",
                    L"Cannot create default classdef object.");
            }
            return result[0];
        }
        if (!result[0].isClassdefType() || result[0].classType() != className) {
            Error(L"Nelson:interpreter:cannotCreateClassdefObject",
                L"Cannot create default classdef object.");
        }
        valueDefaultCache[className] = result[0];
        return result[0];
    }
    //=============================================================================
    bool
    classdefAssignmentClassName(const ArrayOf& value, std::string& className)
    {
        if (value.isClassdefType()) {
            className = value.classType();
            return true;
        }
        if (!value.isHandle()) {
            return false;
        }
        className = ClassName(value);
        return classdefLoadClass(className) && classdefIsHandleClass(className);
    }
    //=============================================================================
    bool
    classdefAssignmentTardimensions(
        const ArrayOf& base, const ArrayOfVector& indices, Dimensions& dimensions)
    {
        if (indices.empty()) {
            return false;
        }
        if (indices.size() == 1) {
            ArrayOf index = indices[0];
            if (isColonIndex(index) || index.isEmpty()) {
                return false;
            }
            index.toOrdinalType();
            indexType maxIndex = index.maxAsIndex();
            if (base.isEmpty() || base.isScalar()) {
                dimensions[0] = 1;
                dimensions[1] = maxIndex;
                return true;
            }
            if (base.isVector()) {
                dimensions = base.dimensions();
                if (base.dimensions()[0] != 1) {
                    dimensions[0] = std::max(dimensions[0], maxIndex);
                } else {
                    dimensions[1] = std::max(dimensions[1], maxIndex);
                }
                return true;
            }
            dimensions[0] = 1;
            dimensions[1] = std::max(base.elementCount(), maxIndex);
            return true;
        }
        dimensions = base.dimensions();
        for (size_t k = 0; k < indices.size(); ++k) {
            ArrayOf index = indices[k];
            if (isColonIndex(index) || index.isEmpty()) {
                return false;
            }
            index.toOrdinalType();
            indexType dimension = static_cast<indexType>(k);
            dimensions[dimension] = std::max(dimensions[dimension], index.maxAsIndex());
        }
        return true;
    }
    //=============================================================================
    // Linear index written by an all-scalar subscript list, in the expanded
    // dimensions; false for colon, empty or vector subscripts.
    bool
    scalarAssignmentTargetIndex(
        const ArrayOfVector& indices, const Dimensions& dimensions, indexType& targetIndex)
    {
        if (indices.empty()) {
            return false;
        }
        Dimensions point(indices.size() == 1 ? dimensions.length() : indices.size());
        for (size_t k = 0; k < indices.size(); ++k) {
            ArrayOf index = indices[k];
            if (isColonIndex(index) || index.isEmpty() || !index.isScalar()) {
                return false;
            }
            index.toOrdinalType();
            point[static_cast<indexType>(k)] = index.maxAsIndex() - 1;
        }
        if (indices.size() == 1) {
            targetIndex = point[0];
            return true;
        }
        Dimensions mapped = dimensions;
        targetIndex = mapped.mapPoint(point);
        return true;
    }
    //=============================================================================
    bool
    expandClassdefAssignmentBaseWithDefaults(
        Evaluator* eval, ArrayOf& base, const ArrayOfVector& indices, const ArrayOf& value)
    {
        std::string className;
        if (!classdefAssignmentClassName(value, className)) {
            return false;
        }
        Dimensions dimensions;
        if (!classdefAssignmentTardimensions(base, indices, dimensions)
            || dimensions.elementCount() == 0) {
            return false;
        }
        // default-filled expansion is only needed when new elements appear:
        // growing a non-empty base, or indexing beyond the values written into
        // an empty base; in-bounds assignments must not rebuild the array
        bool createsHolesFromEmpty
            = base.isEmpty() && dimensions.elementCount() > contentAssignmentTargetCount(indices);
        bool extendsExisting = !base.isEmpty() && dimensions.elementCount() > base.elementCount();
        if (!createsHolesFromEmpty && !extendsExisting) {
            return false;
        }
        // appending the single next element to a vector leaves no hole to fill:
        // no default object is needed, the plain subset assignment grows the
        // array with the written value directly
        if (extendsExisting && (base.isScalar() || base.isVector()) && indices.size() == 1) {
            ArrayOf appendIndex = indices[0];
            if (!isColonIndex(appendIndex) && !appendIndex.isEmpty() && appendIndex.isScalar()) {
                appendIndex.toOrdinalType();
                if (appendIndex.maxAsIndex() == base.elementCount() + 1) {
                    return false;
                }
            }
        }
        if (!base.isEmpty()) {
            std::string baseClassName;
            if (!classdefAssignmentClassName(base, baseClassName) || baseClassName != className) {
                return false;
            }
        }
        ArrayOf defaultObject = evaluateDefaultClassdefObject(eval, className);
        ArrayOf expanded;
        if (value.isClassdefType()) {
            expanded = buildClassdefArrayFromScalar(defaultObject, dimensions);
            copyClassdefBaseIntoExpandedArray(base, expanded);
            base = expanded;
            return true;
        }
        if (!value.isHandle()) {
            return false;
        }
        expanded = buildHandleArrayFromDefaults(eval, className, dimensions, defaultObject);
        // Every element got a filler object, including those overwritten right
        // away by the base copy and by the written value: only the fillers that
        // survive as new elements may later run a destructor.
        std::unordered_set<HandleGenericObject*> fillers;
        bool fillersOwedDestructor = false;
        const indexType expandedCount = expanded.elementCount();
        const auto* refs = static_cast<const HandleRef*>(expanded.dataPointer());
        for (indexType k = 0; k < expandedCount; ++k) {
            HandleGenericObject* filler = refs[k] ? refs[k]->object() : nullptr;
            if (filler != nullptr) {
                fillersOwedDestructor
                    = fillersOwedDestructor || filler->wantsDeferredFinalization();
                filler->setFinalizerRequired(false);
                fillers.insert(filler);
            }
        }
        copyClassdefBaseIntoExpandedArray(base, expanded);
        indexType targetIndex = 0;
        const bool hasScalarTarget = scalarAssignmentTargetIndex(indices, dimensions, targetIndex);
        if (fillersOwedDestructor) {
            refs = static_cast<const HandleRef*>(expanded.dataPointer());
            for (indexType k = 0; k < expandedCount; ++k) {
                if (hasScalarTarget && k == targetIndex) {
                    continue;
                }
                HandleGenericObject* object = refs[k] ? refs[k]->object() : nullptr;
                if (object != nullptr && fillers.find(object) != fillers.end()) {
                    object->setFinalizerRequired(true);
                }
            }
        }
        base = expanded;
        return true;
    }
    //=============================================================================
    // Element write into an existing real double array through a linear index:
    // the whole generic subset-assignment machinery is bypassed, so this must
    // reject anything it would treat differently (growth, logical or non
    // integral indices, type conversion of the target array).
    bool
    tryAssignDoubleScalarLinearDirect(ArrayOf& base, const ArrayOf& index, const ArrayOf& value)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || !value.isDoubleType(true)
            || value.elementCount() != 1) {
            return false;
        }
        indexType ordinal = 0;
        if (!scalarDoubleOrdinalIndex(index, ordinal) || ordinal > base.elementCount()) {
            return false;
        }
        static_cast<double*>(base.readWriteDataPointer())[ordinal - 1]
            = *static_cast<const double*>(value.dataPointer());
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleScalar2DDirect(
        ArrayOf& base, const ArrayOf& rowIndex, const ArrayOf& colIndex, const ArrayOf& value)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || !value.isDoubleType(true)
            || value.elementCount() != 1) {
            return false;
        }
        indexType row = 0;
        indexType col = 0;
        if (!scalarDoubleOrdinalIndex(rowIndex, row) || !scalarDoubleOrdinalIndex(colIndex, col)
            || row > base.dimensionLength(0) || col > base.dimensionLength(1)) {
            return false;
        }
        const indexType offset = (row - 1) + (col - 1) * base.dimensionLength(0);
        static_cast<double*>(base.readWriteDataPointer())[offset]
            = *static_cast<const double*>(value.dataPointer());
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleScalar2D(ArrayOf& base, const ArrayOfVector& indices, ArrayOf& value)
    {
        return indices.size() == 2
            && tryAssignDoubleScalar2DDirect(base, indices[0], indices[1], value);
    }
    //=============================================================================
    // In-place write of a real double scalar into a real double array through N scalar subscripts
    // (N >= 2), e.g. c(i,j,k) = v. Returns false for anything else: non-double base or value,
    // sparse, non-scalar or out-of-range subscript, or growth.
    bool
    tryAssignDoubleScalarNDimDirect(
        ArrayOf& base, const ArrayOfVector& indices, const ArrayOf& value)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || !value.isDoubleType(true)
            || value.elementCount() != 1 || indices.size() < 2) {
            return false;
        }
        // Read extents from a local copy: Dimensions::dimensionLength() is const and
        // yields 1 for a trailing (beyond-rank) subscript, whereas ArrayOf::dimensionLength
        // routes through the mutating Dimensions::operator[], which would grow base in place
        // (e.g. mat(1,1,1)=v on a 1x2 array would wrongly become 1x2x1).
        const Dimensions baseDims = base.dimensions();
        indexType offset = 0;
        indexType stride = 1;
        for (size_t k = 0; k < indices.size(); ++k) {
            indexType ordinal = 0;
            if (!scalarDoubleOrdinalIndex(indices[k], ordinal)) {
                return false;
            }
            const indexType len = baseDims.dimensionLength(static_cast<indexType>(k));
            if (ordinal > len) {
                return false;
            }
            offset += (ordinal - 1) * stride;
            stride *= len;
        }
        static_cast<double*>(base.readWriteDataPointer())[offset]
            = *static_cast<const double*>(value.dataPointer());
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleMatrix2DDirect(
        ArrayOf& base, const ArrayOf& rowIndex, const ArrayOf& colIndex, const ArrayOf& value)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || base.isComplex()
            || value.dataClass() != NLS_DOUBLE || value.isSparse() || value.isComplex()) {
            return false;
        }
        const indexType rows = base.dimensionLength(0);
        const indexType cols = base.dimensionLength(1);
        std::vector<indexType> rowOrdinals;
        std::vector<indexType> colOrdinals;
        if (!doubleOrdinalVectorIndex(rowIndex, rows, rowOrdinals)
            || !doubleOrdinalVectorIndex(colIndex, cols, colOrdinals)) {
            return false;
        }
        const indexType outRows = static_cast<indexType>(rowOrdinals.size());
        const indexType outCols = static_cast<indexType>(colOrdinals.size());
        const indexType targetCount = outRows * outCols;
        if (value.elementCount() != targetCount && value.elementCount() != 1) {
            return false;
        }
        auto* target = static_cast<double*>(base.readWriteDataPointer());
        const auto* source = static_cast<const double*>(value.dataPointer());
        const bool scalarValue = value.elementCount() == 1;
        for (indexType c = 0; c < outCols; ++c) {
            const indexType targetColumnOffset = (colOrdinals[c] - 1) * rows;
            const indexType sourceColumnOffset = c * outRows;
            for (indexType r = 0; r < outRows; ++r) {
                target[targetColumnOffset + rowOrdinals[r] - 1]
                    = source[scalarValue ? 0 : sourceColumnOffset + r];
            }
        }
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleVectorLinearDirect(ArrayOf& base, const ArrayOf& index, const ArrayOf& value)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || base.isComplex()
            || value.dataClass() != NLS_DOUBLE || value.isSparse() || value.isComplex()) {
            return false;
        }
        std::vector<indexType> ordinals;
        if (!doubleOrdinalVectorIndex(index, base.elementCount(), ordinals)) {
            return false;
        }
        const indexType targetCount = static_cast<indexType>(ordinals.size());
        if (value.elementCount() != targetCount && value.elementCount() != 1) {
            return false;
        }
        auto* target = static_cast<double*>(base.readWriteDataPointer());
        const auto* source = static_cast<const double*>(value.dataPointer());
        const bool scalarValue = value.elementCount() == 1;
        for (indexType k = 0; k < targetCount; ++k) {
            target[ordinals[k] - 1] = source[scalarValue ? 0 : k];
        }
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleLogicalMaskDirect(ArrayOf& base, const ArrayOf& mask, const ArrayOf& value)
    {
        if (base.dataClass() != NLS_DOUBLE || base.isSparse() || base.isComplex()
            || mask.dataClass() != NLS_LOGICAL || mask.isSparse() || value.dataClass() != NLS_DOUBLE
            || value.isSparse() || value.isComplex()) {
            return false;
        }
        const indexType maskCount = mask.elementCount();
        if (maskCount > base.elementCount()) {
            return false;
        }
        const auto* maskData = static_cast<const logical*>(mask.dataPointer());
        indexType selectedCount = 0;
        for (indexType k = 0; k < maskCount; ++k) {
            if (maskData[k] != 0) {
                ++selectedCount;
            }
        }
        if (selectedCount == 0) {
            return value.elementCount() == 0 || value.isEmpty();
        }
        if (value.elementCount() != selectedCount && value.elementCount() != 1) {
            return false;
        }
        auto* target = static_cast<double*>(base.readWriteDataPointer());
        const auto* source = static_cast<const double*>(value.dataPointer());
        const bool scalarValue = value.elementCount() == 1;
        indexType out = 0;
        for (indexType k = 0; k < maskCount; ++k) {
            if (maskData[k] != 0) {
                target[k] = source[scalarValue ? 0 : out++];
            }
        }
        return true;
    }
    //=============================================================================
    bool
    isStringScalarDirectAssignmentValue(const ArrayOf& value)
    {
        return (value.isCharacterArray() && value.isRowVector())
            || (value.isStringArray() && value.isScalar());
    }
    //=============================================================================
    ArrayOf
    stringScalarDirectAssignmentElement(const ArrayOf& value)
    {
        if (value.isStringArray()) {
            const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
            return elements[0];
        }
        return value;
    }
    //=============================================================================
    bool
    scalarOrdinalIndex(const ArrayOf& index, indexType& value)
    {
        if (!index.isScalar() || index.isLogical() || index.isCharacterArray()
            || index.isStringArray()) {
            return false;
        }
        try {
            value = index.asScalarIndex(false, true);
        } catch (const NelsonException&) {
            return false;
        }
        return value > 0;
    }
    //=============================================================================
    bool
    tryAssignStructFieldIndexedDirect(ArrayOf& base, const std::string& field,
        const ArrayOfVector& indices, const ArrayOf& value, bool isBraceIndex)
    {
        if (isBraceIndex || indices.size() != 1 || !base.isStruct() || base.isSparse()) {
            return false;
        }
        indexType ordinal = 0;
        if (!scalarDoubleOrdinalIndex(indices[0], ordinal)
            && !scalarOrdinalIndex(indices[0], ordinal)) {
            return false;
        }
        int64 fieldIndex = base.fieldIndex(field);
        if (fieldIndex < 0) {
            stringVector fields = base.fieldNames();
            fields.push_back(field);
            base.promoteType(NLS_STRUCT_ARRAY, fields);
            fieldIndex = base.fieldIndex(field);
            if (fieldIndex < 0) {
                return false;
            }
        }
        if (ordinal > base.elementCount()) {
            ArrayOfVector values;
            stringVector fields = base.fieldNames();
            values.reserve(fields.size());
            for (size_t k = 0; k < fields.size(); ++k) {
                values.push_back(ArrayOf::empty());
            }
            values[static_cast<size_t>(fieldIndex)] = value;
            ArrayOf scalar = ArrayOf::structScalar(fields, values);
            ArrayOf index = indices[0];
            base.setVectorSubset(index, scalar);
            return true;
        }
        indexType fieldCount = static_cast<indexType>(base.fieldNames().size());
        auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        elements[(ordinal - 1) * fieldCount + static_cast<indexType>(fieldIndex)] = value;
        elements[(ordinal - 1) * fieldCount + static_cast<indexType>(fieldIndex)].name("");
        return true;
    }
    //=============================================================================
    bool
    tryStructFieldScalarDoubleRowSentinel(
        const ArrayOf& base, const std::string& field, ArrayOf& result)
    {
        if (!base.isStruct() || base.isSparse()) {
            return false;
        }
        const indexType elementCount = base.elementCount();
        if (elementCount == 0) {
            return false;
        }
        const indexType fieldCount = static_cast<indexType>(base.fieldNames().size());
        const int64 fieldIndex = base.fieldIndex(field);
        if (fieldIndex < 0) {
            return false;
        }
        std::unique_ptr<double[]> data;
        try {
            data = std::make_unique<double[]>(static_cast<size_t>(elementCount));
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
        for (indexType k = 0; k < elementCount; ++k) {
            const ArrayOf& value = elements[k * fieldCount + static_cast<indexType>(fieldIndex)];
            if (!value.isDoubleType(true) || value.elementCount() != 1) {
                return false;
            }
            data[static_cast<size_t>(k)] = *static_cast<const double*>(value.dataPointer());
        }
        Dimensions dimensions(1, elementCount);
        result = ArrayOf(NLS_DOUBLE, dimensions, data.get());
        if (data) {
            data.release();
        }
        result.name(COMMA_LIST_DOUBLE_ROW_SENTINEL_NAME);
        return true;
    }
    //=============================================================================
    bool
    tryAssignStructScalarFieldDirect(ArrayOf& base, const std::string& field, const ArrayOf& value)
    {
        if (!base.isStruct() || !base.isScalar() || base.isSparse()) {
            return false;
        }
        int64 fieldIndex = base.fieldIndex(field);
        if (fieldIndex < 0) {
            return false;
        }
        auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        elements[static_cast<indexType>(fieldIndex)] = value;
        elements[static_cast<indexType>(fieldIndex)].name("");
        return true;
    }
    //=============================================================================
    bool
    tryIncrementStructScalarDoubleFieldDirect(
        ArrayOf& base, const std::string& field, double increment)
    {
        if (!base.isStruct() || !base.isScalar() || base.isSparse()) {
            return false;
        }
        int64 fieldIndex = base.fieldIndex(field);
        if (fieldIndex < 0) {
            return false;
        }
        auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        ArrayOf& fieldValue = elements[static_cast<indexType>(fieldIndex)];
        if (!fieldValue.isDoubleType(true) || fieldValue.elementCount() != 1) {
            return false;
        }
        *static_cast<double*>(fieldValue.readWriteDataPointer()) += increment;
        fieldValue.name("");
        return true;
    }
    //=============================================================================
    bool
    tryIncrementNestedStructScalarDoubleFieldDirect(ArrayOf& base, const std::string& parentField,
        const std::string& childField, double increment)
    {
        if (!base.isStruct() || !base.isScalar() || base.isSparse()) {
            return false;
        }
        int64 parentFieldIndex = base.fieldIndex(parentField);
        if (parentFieldIndex < 0) {
            return false;
        }
        auto* baseElements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        ArrayOf& parentValue = baseElements[static_cast<indexType>(parentFieldIndex)];
        if (!parentValue.isStruct() || !parentValue.isScalar() || parentValue.isSparse()) {
            return false;
        }
        int64 childFieldIndex = parentValue.fieldIndex(childField);
        if (childFieldIndex < 0) {
            return false;
        }
        auto* parentElements = static_cast<ArrayOf*>(parentValue.readWriteDataPointer());
        ArrayOf& childValue = parentElements[static_cast<indexType>(childFieldIndex)];
        if (!childValue.isDoubleType(true) || childValue.elementCount() != 1) {
            return false;
        }
        *static_cast<double*>(childValue.readWriteDataPointer()) += increment;
        childValue.name("");
        parentValue.name("");
        return true;
    }
    //=============================================================================
    bool
    tryAssignNestedStructScalarFieldDirect(ArrayOf& base, const std::string& parentField,
        const std::string& childField, const ArrayOf& value)
    {
        if (!base.isStruct() || !base.isScalar() || base.isSparse()) {
            return false;
        }
        int64 parentFieldIndex = base.fieldIndex(parentField);
        if (parentFieldIndex < 0) {
            return false;
        }
        auto* baseElements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        ArrayOf& parentValue = baseElements[static_cast<indexType>(parentFieldIndex)];
        if (!parentValue.isStruct() || !parentValue.isScalar() || parentValue.isSparse()) {
            return false;
        }
        int64 childFieldIndex = parentValue.fieldIndex(childField);
        if (childFieldIndex < 0) {
            return false;
        }
        auto* parentElements = static_cast<ArrayOf*>(parentValue.readWriteDataPointer());
        parentElements[static_cast<indexType>(childFieldIndex)] = value;
        parentElements[static_cast<indexType>(childFieldIndex)].name("");
        parentValue.name("");
        return true;
    }
    //=============================================================================
    bool
    tryReadStructScalarFieldDirect(const ArrayOf& base, const std::string& field, ArrayOf& value)
    {
        if (!base.isStruct() || !base.isScalar() || base.isSparse()) {
            return false;
        }
        int64 fieldIndex = base.fieldIndex(field);
        if (fieldIndex < 0) {
            return false;
        }
        const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
        value = elements[static_cast<indexType>(fieldIndex)];
        return true;
    }
    //=============================================================================
    ArrayOf
    readDotValueGeneric(Evaluator* eval, const ArrayOf& base, const std::string& field)
    {
        ArrayOf directValue;
        if (tryReadStructScalarFieldDirect(base, field, directValue)
            || tryFastTableDotVariable(eval, base, field, directValue)) {
            return directValue;
        }
        if (base.isClassType()) {
            if (tryReadClassdefPlainProperty(eval, base, field, directValue)) {
                return directValue;
            }
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back(".");
            subsindices.push_back(ArrayOf::characterArray(field));
            bool haveFunction = false;
            ArrayOfVector result
                = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction, 1);
            if (!haveFunction) {
                result = getClassdefFieldAsList(eval, base, field);
            }
            return result.empty() ? ArrayOf::empty() : result[0];
        }
        if (base.isHandle() || base.isGraphicsObject()) {
            ArrayOfVector params;
            ArrayOfVector result = getOrInvokeHandle(eval, base, field, params);
            return result.empty() ? ArrayOf::empty() : result[0];
        }
        if (!base.isStruct()) {
            Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
        }
        ArrayOfVector fieldValues = base.fieldAsList(field);
        return fieldValues.empty() ? ArrayOf::empty() : fieldValues[0];
    }
    //=============================================================================
    // Read an intermediate field while building a chain such as `s.parent.child = value`. A
    // missing field on a struct (or on a freshly seeded empty base) gives an empty value that the
    // next assign turns into a struct, instead of "Reference to non-existent field".
    ArrayOf
    readDotValueForAssign(Evaluator* eval, const ArrayOf& base, const std::string& field)
    {
        if (base.isStruct()) {
            if (!base.isField(field)) {
                return ArrayOf::empty();
            }
        } else if (base.isEmpty() && !base.isClassType()) {
            return ArrayOf::empty();
        }
        checkDotAssignParent(base);
        return readDotValueGeneric(eval, base, field);
    }
    //=============================================================================
    // obj.field(args) inside a method of a classic (struct-based) object indexes
    // the value of its data field.
    ArrayOf
    subsetClassicFieldValue(Evaluator* eval, ArrayOf fieldValue, ArrayOfVector idx)
    {
        resolveObjectIndices(eval, idx);
        if (idx.size() > 1) {
            for (size_t ci = 0; ci < idx.size(); ++ci) {
                if (isColonIndex(idx[ci])) {
                    idx[ci] = ArrayOf::integerRange(
                        1, 1, fieldValue.dimensionLength(static_cast<int>(ci)), false);
                }
            }
        }
        return idx.size() == 1 ? fieldValue.vectorSubset(idx[0]) : fieldValue.nDimSubset(idx);
    }
    //=============================================================================
    void
    assignDotValueGeneric(
        Evaluator* eval, ArrayOf& base, const std::string& field, const ArrayOf& value)
    {
        ArrayOfVector values;
        values.push_back(value);
        if (base.isClassType() && base.dataClass() == NLS_CLASS_ARRAY
            && eval->getClassdefAccessContext() == base.classType()) {
            // Inside its own method, a classic (struct-based) object writes its
            // data field directly rather than through its overloaded subsasgn.
            base.setFieldAsList(field, values);
            return;
        }
        if (tryFastTableDotAssign(eval, base, field, value)) {
            return;
        }
        if (base.isClassType()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back(".");
            subsindices.push_back(ArrayOf::characterArray(field));
            bool haveFunction = false;
            ArrayOf assigned
                = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
            if (!haveFunction) {
                Error(L"Nelson:interpreter:invalidFieldAssignment",
                    L"Cannot apply A.field_name = B to non struct-array object A.");
            }
            base = assigned;
            return;
        }
        if (base.isHandle() || base.isGraphicsObject()) {
            eval->bytecodeSetHandle(base, field, values);
            return;
        }
        checkDotAssignTarget(base);
        if (tryAssignStructScalarFieldDirect(base, field, value)) {
            return;
        }
        if (base.isEmpty() || (base.isStruct() && base.isScalar())) {
            ArrayOf copy = value;
            base.setField(field, copy);
            return;
        }
        base.setFieldAsList(field, values);
    }
    //=============================================================================
    // Read one nelson.indexing.IndexingOperation element into a subtype ("()",
    // "{}" or ".") plus its indices (paren/brace) or field name (dot).
    void
    readIndexingOp(Evaluator* eval, const ArrayOf& op, std::string& subtype, ArrayOfVector& indices,
        std::string& name)
    {
        ArrayOfVector typeList = getClassdefFieldAsList(eval, op, "Type");
        if (typeList.empty()) {
            Error(L"Nelson:interpreter:invalidIndexingOperation", L"Invalid indexing operation.");
        }
        ArrayOfVector typeNameList = getClassdefFieldAsList(eval, typeList[0], "Name");
        const std::string typeName
            = typeNameList.empty() ? std::string() : typeNameList[0].asUtf8String();
        if (typeName == "Paren") {
            subtype = "()";
        } else if (typeName == "Brace") {
            subtype = "{}";
        } else if (typeName == "Dot") {
            subtype = ".";
        } else {
            Error(L"Nelson:interpreter:unsupportedIndexingOperation",
                L"Unsupported indexing operation type.");
        }
        if (subtype == ".") {
            ArrayOfVector nameList = getClassdefFieldAsList(eval, op, "Name");
            name = nameList.empty() ? std::string() : nameList[0].asUtf8String();
            return;
        }
        ArrayOfVector indicesList = getClassdefFieldAsList(eval, op, "Indices");
        if (!indicesList.empty() && indicesList[0].isCell()) {
            ArrayOf cell = indicesList[0];
            indexType count = cell.elementCount();
            const auto* elements = static_cast<const ArrayOf*>(cell.dataPointer());
            for (indexType i = 0; elements != nullptr && i < count; ++i) {
                indices.push_back(elements[i]);
            }
        }
    }
    //=============================================================================
    ArrayOf
    indexingOpElement(const ArrayOf& opsValue, indexType count, indexType k)
    {
        if (count == 1) {
            return opsValue;
        }
        ArrayOf opsCopy = opsValue;
        ArrayOf index = ArrayOf::doubleScalar(static_cast<double>(k + 1));
        return opsCopy.vectorSubset(index);
    }
    //=============================================================================
    // Apply a single indexing operation (read) to a value, mirroring the VM's
    // per-op primitives so any base type (classdef, struct, cell, array) works.
    ArrayOf
    applyIndexingReadStep(Evaluator* eval, const ArrayOf& current, const std::string& subtype,
        ArrayOfVector& indices, const std::string& name)
    {
        if (subtype == ".") {
            return readDotValueGeneric(eval, current, name);
        }
        ArrayOf base = current;
        if (base.isClassType()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back(subtype);
            subsindices.push_back(classIndexCell(indices));
            bool haveFunction = false;
            ArrayOfVector result
                = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction, 1);
            if (haveFunction) {
                return result.empty() ? ArrayOf::empty() : result[0];
            }
        }
        if (subtype == "{}") {
            ArrayOfVector contents = indices.size() == 1 ? base.vectorContentsAsList(indices[0])
                                                         : base.nDimContentsAsList(indices);
            return contents.empty() ? ArrayOf::empty() : contents[0];
        }
        return indices.size() == 1 ? base.vectorSubset(indices[0]) : base.nDimSubset(indices);
    }
    //=============================================================================
    ArrayOf
    applyIndexingOperationRead(Evaluator* eval, const ArrayOf& base, const ArrayOf& opsValue)
    {
        indexType nops = opsValue.elementCount();
        ArrayOf current = base;
        for (indexType k = 0; k < nops; ++k) {
            ArrayOf op = indexingOpElement(opsValue, nops, k);
            std::string subtype;
            std::string name;
            ArrayOfVector indices;
            readIndexingOp(eval, op, subtype, indices, name);
            current = applyIndexingReadStep(eval, current, subtype, indices, name);
        }
        return current;
    }
    //=============================================================================
    // Apply a single indexing operation (write) of rhs into base, returning the
    // updated base (value semantics).
    ArrayOf
    applyIndexingWriteStep(Evaluator* eval, ArrayOf base, const std::string& subtype,
        ArrayOfVector& indices, const std::string& name, const ArrayOf& rhs)
    {
        if (subtype == ".") {
            assignDotValueGeneric(eval, base, name, rhs);
            return base;
        }
        if (base.isClassType()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back(subtype);
            subsindices.push_back(classIndexCell(indices));
            bool haveFunction = false;
            ArrayOf assigned
                = eval->bytecodeAssignClass(base, subtypes, subsindices, rhs, haveFunction);
            if (haveFunction) {
                return assigned;
            }
        }
        ArrayOf value = rhs;
        if (subtype == "{}") {
            ArrayOfVector values;
            values.push_back(value);
            if (indices.size() == 1) {
                base.setVectorContentsAsList(indices[0], values);
            } else {
                base.setNDimContentsAsList(indices, values);
            }
            return base;
        }
        if (indices.size() == 1) {
            base.setVectorSubset(indices[0], value);
        } else {
            base.setNDimSubset(indices, value);
        }
        return base;
    }
    //=============================================================================
    // Multi-op chains recurse: read the prefix, assign the tail, write back.
    ArrayOf
    applyIndexingOperationWrite(Evaluator* eval, const ArrayOf& base, const ArrayOf& opsValue,
        indexType opIndex, const ArrayOf& rhs)
    {
        indexType nops = opsValue.elementCount();
        ArrayOf op = indexingOpElement(opsValue, nops, opIndex);
        std::string subtype;
        std::string name;
        ArrayOfVector indices;
        readIndexingOp(eval, op, subtype, indices, name);
        if (opIndex + 1 >= nops) {
            return applyIndexingWriteStep(eval, base, subtype, indices, name, rhs);
        }
        ArrayOf child = applyIndexingReadStep(eval, base, subtype, indices, name);
        ArrayOf updatedChild = applyIndexingOperationWrite(eval, child, opsValue, opIndex + 1, rhs);
        ArrayOfVector idxCopy = indices;
        return applyIndexingWriteStep(eval, base, subtype, idxCopy, name, updatedChild);
    }
    //=============================================================================
    bool
    tryAssignStringScalarElementAtOffset(ArrayOf& base, indexType offset, const ArrayOf& element)
    {
        if (!base.isStringArray() || base.isSparse() || !element.isCharacterArray()
            || !element.isRowVector() || offset < 0 || offset >= base.elementCount()) {
            return false;
        }
        auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
        const ArrayOf& current = elements[offset];
        if (current.isCharacterArray() && current.elementCount() == element.elementCount()
            && current.dataPointer() == element.dataPointer()) {
            return true;
        }
        elements[offset] = element;
        return true;
    }
    //=============================================================================
    bool
    tryAssignStringScalarAtOffset(ArrayOf& base, indexType offset, const ArrayOf& value)
    {
        if (!isStringScalarDirectAssignmentValue(value)) {
            return false;
        }
        return tryAssignStringScalarElementAtOffset(
            base, offset, stringScalarDirectAssignmentElement(value));
    }
    //=============================================================================
    bool
    tryAssignStringScalarAllElements(ArrayOf& base, const ArrayOf& value)
    {
        if (!base.isStringArray() || base.isSparse()
            || !isStringScalarDirectAssignmentValue(value)) {
            return false;
        }
        ArrayOf element = stringScalarDirectAssignmentElement(value);
        if (!element.isCharacterArray() || !element.isRowVector()) {
            return false;
        }
        return replaceArrayOfElementsWithScalar(base, NLS_STRING_ARRAY, element);
    }
    //=============================================================================
    bool
    tryAssignStringScalarElementAtOrdinal(ArrayOf& base, uint16_t ordinal, const ArrayOf& element)
    {
        if (ordinal == 0) {
            return false;
        }
        return tryAssignStringScalarElementAtOffset(
            base, static_cast<indexType>(ordinal) - 1, element);
    }
    //=============================================================================
    bool
    tryAssignStringScalarLinearDirect(ArrayOf& base, const ArrayOf& index, const ArrayOf& value)
    {
        indexType ordinal = 0;
        if (!scalarOrdinalIndex(index, ordinal)) {
            return false;
        }
        return tryAssignStringScalarAtOffset(base, ordinal - 1, value);
    }
    //=============================================================================
    bool
    tryAssignStringScalar2DDirect(
        ArrayOf& base, const ArrayOf& rowIndex, const ArrayOf& colIndex, const ArrayOf& value)
    {
        indexType row = 0;
        indexType col = 0;
        if (!scalarOrdinalIndex(rowIndex, row) || !scalarOrdinalIndex(colIndex, col)) {
            return false;
        }
        if (row > base.dimensionLength(0) || col > base.dimensionLength(1)) {
            return false;
        }
        indexType offset = (row - 1) + (col - 1) * base.dimensionLength(0);
        return tryAssignStringScalarAtOffset(base, offset, value);
    }
    //=============================================================================
    bool
    tryAssignStringScalarNDimDirect(
        ArrayOf& base, const ArrayOfVector& indices, const ArrayOf& value)
    {
        if (!base.isStringArray() || base.isSparse() || !isStringScalarDirectAssignmentValue(value)
            || indices.empty()) {
            return false;
        }
        if (indices.size() == 1) {
            return tryAssignStringScalarLinearDirect(base, indices[0], value);
        }
        indexType offset = 0;
        indexType stride = 1;
        for (size_t k = 0; k < indices.size(); ++k) {
            indexType ordinal = 0;
            if (!scalarOrdinalIndex(indices[k], ordinal)) {
                return false;
            }
            if (ordinal > base.dimensionLength(static_cast<int>(k))) {
                return false;
            }
            offset += (ordinal - 1) * stride;
            stride *= base.dimensionLength(static_cast<int>(k));
        }
        return tryAssignStringScalarAtOffset(base, offset, value);
    }
    //=============================================================================
    ArrayOf
    constantAsArray(const ConstantValue& value)
    {
        if (const auto* arrayValue = std::get_if<ArrayOf>(&value)) {
            return *arrayValue;
        }
        Error(L"Nelson:bytecode:invalidConstant", L"Bytecode constant is not an ArrayOf value.");
        return ArrayOf::empty();
    }
    //=============================================================================
    bool
    canDirectSelfBytecodeCall(Evaluator* eval, const BytecodeChunk& callerChunk,
        FunctionDef* funcDef, const ArrayOf* args, size_t nargs, uint16_t nout,
        BytecodeChunk*& calleeChunk)
    {
        calleeChunk = nullptr;
        if (funcDef == nullptr || funcDef != callerChunk.selfFunction
            || funcDef->type() != NLS_MACRO_FUNCTION || eval->isBreakpointActive() || eval->stepMode
            || eval->hasEnabledBreakpoints() || !eval->commandQueue.isEmpty()
            || Profiler::getInstance()->isOn()) {
            return false;
        }
        auto* macroDef = dynamic_cast<MacroFunctionDef*>(funcDef);
        if (macroDef == nullptr || macroDef->getIsScript() || macroDef->localFunction
            || macroDef->nestedFunction || !macroDef->classdefOwnerClassName.empty()
            || macroDef->bytecodeChunk == nullptr) {
            return false;
        }
        if (eval->withOverload && macroDef->overloadAutoMode == NLS_OVERLOAD_AUTO_ON && nargs > 0
            && (args[0].isFunctionHandle() || hasOverloadDispatchCandidate(args, nargs))) {
            return false;
        }
        const BytecodeChunk& directChunk = *macroDef->bytecodeChunk;
        if (directChunk.usesNarginNargout || directChunk.hasDeclareImport) {
            return false;
        }
        if (macroDef->inputArgCount() != -1 && nargs == 0 && !macroDef->arguments.empty()) {
            return false;
        }
        if (nout == 1 && nargs > 0 && directChunk.argNames.size() == 1
            && directChunk.argNames[0] != "varargin" && directChunk.retNames.size() == 1) {
            calleeChunk = macroDef->bytecodeChunk.get();
            return true;
        }
        calleeChunk = macroDef->bytecodeChunk.get();
        return true;
    }
    //=============================================================================
    bool
    canDirectLeafMacroBytecodeCall(Evaluator* eval, FunctionDef* funcDef, const ArrayOfVector& args,
        uint16_t nout, BytecodeChunk*& calleeChunk)
    {
        calleeChunk = nullptr;
        if (funcDef == nullptr || funcDef->type() != NLS_MACRO_FUNCTION || nout != 1
            || args.size() != 1 || eval->isBreakpointActive() || eval->stepMode
            || eval->hasEnabledBreakpoints() || !eval->commandQueue.isEmpty()
            || Profiler::getInstance()->isOn()) {
            return false;
        }
        auto* macroDef = dynamic_cast<MacroFunctionDef*>(funcDef);
        if (macroDef == nullptr || macroDef->getIsScript() || macroDef->nestedFunction
            || !macroDef->classdefOwnerClassName.empty()) {
            return false;
        }
        if (eval->withOverload && macroDef->overloadAutoMode == NLS_OVERLOAD_AUTO_ON
            && (args[0].isClassType() || args[0].isHandle() || args[0].isFunctionHandle())) {
            return false;
        }
        macroDef->ensureBytecodeCompiled();
        if (macroDef->bytecodeChunk == nullptr) {
            return false;
        }
        if (macroDef->inputArgCount() != -1 && args.size() > macroDef->arguments.size()) {
            Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
        }
        BytecodeChunk& directChunk = *macroDef->bytecodeChunk;
        if (directChunk.argNames.size() != 1 || directChunk.retNames.size() != 1
            || directChunk.argNames[0] == "varargin" || directChunk.retNames[0] == "varargout") {
            return false;
        }
        if (directChunk.usesNarginNargout || !directChunk.directLeafOpsAllowed) {
            return false;
        }
        calleeChunk = macroDef->bytecodeChunk.get();
        return true;
    }
    //=============================================================================
    // Transitive walk answering "does calling funcDef require the caller's
    // context scope to be present and synced (refresh=false) or refreshed after
    // the call (refresh=true)". Results are cached on the callee chunks, keyed
    // on FunctionsInMemory::generation() so that any function-table mutation
    // (which can rebind the names this walk resolved) invalidates them.
    bool
    functionNeedsCallerContextWalk(Evaluator* eval, FunctionDef* funcDef,
        std::unordered_set<FunctionDef*>& visiting, bool refreshRequired, uint64 generation)
    {
        if (funcDef == nullptr) {
            return true;
        }
        if (funcDef->type() == NLS_BUILT_IN_FUNCTION) {
            auto* builtinDef = dynamic_cast<BuiltInFunctionDef*>(funcDef);
            if (builtinDef == nullptr) {
                return true;
            }
            return refreshRequired ? builtinDef->mustRefreshCallerContext()
                                   : builtinDef->mustSyncCallerContext();
        }
        BytecodeChunk* calleeChunkPtr = nullptr;
        if (funcDef->type() == NLS_MACRO_FUNCTION) {
            auto* macroDef = dynamic_cast<MacroFunctionDef*>(funcDef);
            if (macroDef == nullptr || macroDef->getIsScript() || macroDef->nestedFunction) {
                return true;
            }
            if (!visiting.insert(funcDef).second) {
                return false;
            }
            macroDef->ensureBytecodeCompiled();
            if (macroDef->bytecodeChunk == nullptr) {
                return true;
            }
            calleeChunkPtr = macroDef->bytecodeChunk.get();
        } else if (funcDef->type() == NLS_ANONYMOUS_MACRO_FUNCTION) {
            auto* anonymousDef = dynamic_cast<AnonymousMacroFunctionDef*>(funcDef);
            if (anonymousDef == nullptr) {
                return true;
            }
            if (anonymousDef->isStatelessSimpleIdentity()) {
                return false;
            }
            if (anonymousDef->isFunctionHandle()) {
                FunctionDef* targetDef = nullptr;
                if (!eval->lookupFunction(anonymousDef->getContent(), targetDef)) {
                    return true;
                }
                return functionNeedsCallerContextWalk(
                    eval, targetDef, visiting, refreshRequired, generation);
            }
            if (!visiting.insert(funcDef).second) {
                return false;
            }
            anonymousDef->ensureBytecodeCompiled(1);
            if (anonymousDef->bytecodeChunk == nullptr) {
                return true;
            }
            calleeChunkPtr = anonymousDef->bytecodeChunk.get();
        } else {
            return true;
        }
        if (refreshRequired && calleeChunkPtr->callerContextRefreshCacheValid
            && calleeChunkPtr->callerContextRefreshCacheGeneration == generation) {
            return calleeChunkPtr->callerContextRefreshRequired;
        }
        if (!refreshRequired && calleeChunkPtr->callerContextSyncCacheValid
            && calleeChunkPtr->callerContextSyncCacheGeneration == generation) {
            return calleeChunkPtr->callerContextSyncRequired;
        }
        bool result = false;
        const BytecodeChunk& calleeChunk = *calleeChunkPtr;
        for (const Instruction& calleeIns : calleeChunk.code) {
            if (calleeIns.op == OpCode::DECLARE_IMPORT) {
                result = true;
                break;
            }
            if (calleeIns.op != OpCode::CALL_NAMED && calleeIns.op != OpCode::MAKE_FH_NAMED) {
                continue;
            }
            if (calleeIns.A >= calleeChunk.names.size()) {
                result = true;
                break;
            }
            const std::string& calleeName = calleeChunk.names[calleeIns.A];
            FunctionDef* nestedDef = nullptr;
            if (!eval->lookupFunction(calleeName, nestedDef)) {
                // Classdef/container helper builtins emitted by a constructor or method (e.g.
                // __classdef_handle__) are caller-context neutral but do not resolve from the
                // walk's scope. Treating them as needing the caller context synced every caller
                // local on each construction, which made creating N objects in a container O(n^2).
                // Same rule as BuiltInFunctionDefManager.
                if (calleeName.rfind("__classdef_", 0) == 0 || calleeName == "__containers_map__") {
                    continue;
                }
                // The name does not resolve from here (the usual case for a private callee, whose
                // directory is not visible from the caller's scope). Answer conservatively and
                // record the answer, so the walk is not repeated on every call.
                result = true;
                break;
            }
            if (functionNeedsCallerContextWalk(
                    eval, nestedDef, visiting, refreshRequired, generation)) {
                result = true;
                break;
            }
        }
        if (refreshRequired) {
            calleeChunkPtr->callerContextRefreshRequired = result;
            calleeChunkPtr->callerContextRefreshCacheValid = true;
            calleeChunkPtr->callerContextRefreshCacheGeneration = generation;
        } else {
            calleeChunkPtr->callerContextSyncRequired = result;
            calleeChunkPtr->callerContextSyncCacheValid = true;
            calleeChunkPtr->callerContextSyncCacheGeneration = generation;
        }
        return result;
    }
    //=============================================================================
    bool
    loadScalarDoubleConstant(const BytecodeChunk& chunk, uint16_t constantIndex, double& value)
    {
        ArrayOf constant = constantAsArray(chunk.constants.get(constantIndex));
        if (!constant.isDoubleType(true) || constant.elementCount() != 1) {
            return false;
        }
        value = *static_cast<const double*>(constant.dataPointer());
        return true;
    }
    //=============================================================================
    bool
    isAssignedLocalSlot(const BytecodeChunk& chunk, size_t slot)
    {
        return slot < chunk.localAssigned.size() && chunk.localAssigned[slot] != 0;
    }
    //=============================================================================
    bool
    isRuntimeAssignedLocalSlot(const VMCallFrame& frame, size_t slot)
    {
        return slot < frame.assignedLocals.size() && frame.assignedLocals[slot] != 0;
    }
    //=============================================================================
    void
    markRuntimeAssigned(VMCallFrame& frame, size_t slot)
    {
        if (slot < frame.assignedLocals.size()) {
            frame.assignedLocals[slot] = 1;
        }
    }
    //=============================================================================
    enum class ScalarExprOutcome
    {
        // Typed fast path refused: the caller replays the original window.
        Refused,
        Stored,
        BranchTaken,
        BranchNotTaken
    };
    //=============================================================================
    ScalarExprOutcome
    tryExecuteScalarExprProgram(VMCallFrame& frame, const BytecodeChunk& chunk, Context* context,
        std::vector<ArrayOf>& stack, const ScalarExprProgram& program)
    {
        if (trySkipNumericSelfAssignment(frame, chunk, context, program)) {
            return ScalarExprOutcome::Stored;
        }
        double registers[16];
        bool comparison = false;
        int top = 0;
        for (const ScalarExprMicroOp& op : program.ops) {
            switch (op.kind) {
            case ScalarExprOpKind::LoadLocal: {
                if (op.slot >= frame.locals.size() || !isRuntimeAssignedLocalSlot(frame, op.slot)) {
                    return ScalarExprOutcome::Refused;
                }
                const ArrayOf& value = frame.locals[op.slot];
                if (!value.isDoubleType(true) || value.elementCount() != 1) {
                    return ScalarExprOutcome::Refused;
                }
                registers[top++] = *static_cast<const double*>(value.dataPointer());
            } break;
            case ScalarExprOpKind::LoadConst:
                registers[top++] = op.imm;
                break;
            case ScalarExprOpKind::LoadIndexedLocal: {
                if (op.slot >= frame.locals.size() || op.slot2 >= frame.locals.size()
                    || !isRuntimeAssignedLocalSlot(frame, op.slot)
                    || !isRuntimeAssignedLocalSlot(frame, op.slot2)) {
                    return ScalarExprOutcome::Refused;
                }
                const ArrayOf& base = frame.locals[op.slot];
                if (base.dataClass() != NLS_DOUBLE || base.isSparse()) {
                    return ScalarExprOutcome::Refused;
                }
                indexType ordinal = 0;
                if (!scalarDoubleOrdinalIndex(frame.locals[op.slot2], ordinal)
                    || ordinal > base.elementCount()) {
                    return ScalarExprOutcome::Refused;
                }
                registers[top++] = static_cast<const double*>(base.dataPointer())[ordinal - 1];
            } break;
            case ScalarExprOpKind::LoadIndexedLocalOffset: {
                if (op.slot >= frame.locals.size() || op.slot2 >= frame.locals.size()
                    || !isRuntimeAssignedLocalSlot(frame, op.slot)
                    || !isRuntimeAssignedLocalSlot(frame, op.slot2)) {
                    return ScalarExprOutcome::Refused;
                }
                const ArrayOf& base = frame.locals[op.slot];
                if (base.dataClass() != NLS_DOUBLE || base.isSparse()) {
                    return ScalarExprOutcome::Refused;
                }
                if (!frame.locals[op.slot2].isDoubleType(true)
                    || frame.locals[op.slot2].elementCount() != 1) {
                    return ScalarExprOutcome::Refused;
                }
                const double scalarIndex
                    = *static_cast<const double*>(frame.locals[op.slot2].dataPointer()) + op.imm;
                const auto ordinal = static_cast<indexType>(scalarIndex);
                if (scalarIndex < 1.0 || scalarIndex != static_cast<double>(ordinal)
                    || ordinal > base.elementCount()) {
                    return ScalarExprOutcome::Refused;
                }
                registers[top++] = static_cast<const double*>(base.dataPointer())[ordinal - 1];
            } break;
            case ScalarExprOpKind::LoadIndexedLocalMinusLocal: {
                if (op.slot >= frame.locals.size() || op.slot2 >= frame.locals.size()
                    || op.slot3 >= frame.locals.size()
                    || !isRuntimeAssignedLocalSlot(frame, op.slot)
                    || !isRuntimeAssignedLocalSlot(frame, op.slot2)
                    || !isRuntimeAssignedLocalSlot(frame, op.slot3)) {
                    return ScalarExprOutcome::Refused;
                }
                const ArrayOf& base = frame.locals[op.slot];
                if (base.dataClass() != NLS_DOUBLE || base.isSparse()) {
                    return ScalarExprOutcome::Refused;
                }
                if (!frame.locals[op.slot2].isDoubleType(true)
                    || frame.locals[op.slot2].elementCount() != 1
                    || !frame.locals[op.slot3].isDoubleType(true)
                    || frame.locals[op.slot3].elementCount() != 1) {
                    return ScalarExprOutcome::Refused;
                }
                const double scalarIndex
                    = *static_cast<const double*>(frame.locals[op.slot2].dataPointer())
                    - *static_cast<const double*>(frame.locals[op.slot3].dataPointer());
                const auto ordinal = static_cast<indexType>(scalarIndex);
                if (scalarIndex < 1.0 || scalarIndex != static_cast<double>(ordinal)
                    || ordinal > base.elementCount()) {
                    return ScalarExprOutcome::Refused;
                }
                registers[top++] = static_cast<const double*>(base.dataPointer())[ordinal - 1];
            } break;
            case ScalarExprOpKind::Add:
                --top;
                registers[top - 1] += registers[top];
                break;
            case ScalarExprOpKind::Sub:
                --top;
                registers[top - 1] -= registers[top];
                break;
            case ScalarExprOpKind::Mul:
                --top;
                registers[top - 1] *= registers[top];
                break;
            case ScalarExprOpKind::Div:
                --top;
                registers[top - 1] /= registers[top];
                break;
            case ScalarExprOpKind::Neg:
                registers[top - 1] = -registers[top - 1];
                break;
            // NaN comparisons follow IEEE: all false except !=.
            case ScalarExprOpKind::CmpLt:
                --top;
                comparison = registers[top - 1] < registers[top];
                break;
            case ScalarExprOpKind::CmpLe:
                --top;
                comparison = registers[top - 1] <= registers[top];
                break;
            case ScalarExprOpKind::CmpGt:
                --top;
                comparison = registers[top - 1] > registers[top];
                break;
            case ScalarExprOpKind::CmpGe:
                --top;
                comparison = registers[top - 1] >= registers[top];
                break;
            case ScalarExprOpKind::CmpEq:
                --top;
                comparison = registers[top - 1] == registers[top];
                break;
            case ScalarExprOpKind::CmpNe:
                --top;
                comparison = registers[top - 1] != registers[top];
                break;
            }
        }
        if (program.resultKind == ScalarExprResultKind::Branch) {
            bool jump = false;
            switch (program.branchOp) {
            case OpCode::WHILE_CHECK:
            case OpCode::JUMP_IF_FALSE:
            case OpCode::JUMP_IF_FALSE_SCALAR:
                jump = !comparison;
                break;
            case OpCode::JUMP_IF_TRUE:
            case OpCode::JUMP_IF_TRUE_SCALAR:
                jump = comparison;
                break;
            default:
                return ScalarExprOutcome::Refused;
            }
            return jump ? ScalarExprOutcome::BranchTaken : ScalarExprOutcome::BranchNotTaken;
        }
        if (program.resultKind == ScalarExprResultKind::Push) {
            stack.push_back(ArrayOf::doubleScalar(registers[0]));
            return ScalarExprOutcome::Stored;
        }
        if (program.resultKind == ScalarExprResultKind::IndexedStore) {
            if (program.storeSlot >= frame.locals.size() || program.indexSlot >= frame.locals.size()
                || !isRuntimeAssignedLocalSlot(frame, program.storeSlot)
                || !isRuntimeAssignedLocalSlot(frame, program.indexSlot)) {
                return ScalarExprOutcome::Refused;
            }
            if (program.storeSlot < chunk.localNames.size()
                && !chunk.localNames[program.storeSlot].empty()
                && context->isLockedVariable(chunk.localNames[program.storeSlot])) {
                return ScalarExprOutcome::Refused;
            }
            ArrayOf& base = frame.locals[program.storeSlot];
            if (base.dataClass() != NLS_DOUBLE || base.isSparse()) {
                return ScalarExprOutcome::Refused;
            }
            indexType ordinal = 0;
            if (!scalarDoubleOrdinalIndex(frame.locals[program.indexSlot], ordinal)
                || ordinal > base.elementCount()) {
                return ScalarExprOutcome::Refused;
            }
            static_cast<double*>(base.readWriteDataPointer())[ordinal - 1] = registers[0];
            markRuntimeAssigned(frame, program.storeSlot);
            return ScalarExprOutcome::Stored;
        }
        if (program.storeSlot >= frame.locals.size()) {
            return ScalarExprOutcome::Refused;
        }
        if (program.storeSlot < chunk.localNames.size()
            && !chunk.localNames[program.storeSlot].empty()
            && context->isLockedVariable(chunk.localNames[program.storeSlot])) {
            return ScalarExprOutcome::Refused;
        }
        frame.locals[program.storeSlot] = ArrayOf::doubleScalar(registers[0]);
        markRuntimeAssigned(frame, program.storeSlot);
        return ScalarExprOutcome::Stored;
    }
    //=============================================================================
    void
    markRuntimeUnassigned(VMCallFrame& frame, size_t slot)
    {
        if (slot < frame.assignedLocals.size()) {
            frame.assignedLocals[slot] = 0;
        }
    }
    //=============================================================================
    // Number of outputs a comma-separated-list indexing must produce, as reported
    // by the class method numArgumentsFromSubscript. Returns -1 when the class
    // does not define it, so the caller keeps its own default output count.
    static int
    objectNumArgumentsFromSubscript(Evaluator* eval, const ArrayOf& r, const stringVector& subtypes,
        const ArrayOfVector& subsindices, const char* contextMember)
    {
        if (!(r.isClassType() || r.isHandle())) {
            return -1;
        }
        std::string currentClass = r.isHandle() ? r.handleCategory() : ClassName(r);
        if (!classdefLoadClass(currentClass)
            || !classdefHasMethod(currentClass, "numArgumentsFromSubscript")) {
            return -1;
        }
        // build the substruct argument (fields "type"/"subs"), same shape as
        // Evaluator::extractClass builds for subsref
        stringVector fieldnames = { "type", "subs" };
        Dimensions dims(1, subtypes.size());
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false));
        ArrayOf substruct(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        ArrayOfVector typesVector;
        ArrayOfVector subsVector;
        for (const auto& t : subtypes) {
            typesVector.push_back(ArrayOf::characterArray(t));
        }
        for (const auto& s : subsindices) {
            subsVector.push_back(s);
        }
        if (!typesVector.empty()) {
            substruct.setFieldAsList("type", typesVector);
            substruct.setFieldAsList("subs", subsVector);
        }
        // materialize nelson.mixin.util.IndexingContext.<contextMember>
        ArrayOf contextValue;
        std::string enumFunctionName;
        if (classdefResolveEnumerationMemberFunction(
                "nelson.mixin.util.IndexingContext", contextMember, enumFunctionName)) {
            FunctionDef* enumDef = nullptr;
            if (eval->lookupFunction(enumFunctionName, enumDef)) {
                ArrayOfVector enumResult = enumDef->evaluateFunction(eval, {}, 1);
                if (!enumResult.empty()) {
                    contextValue = enumResult[0];
                }
            }
        }
        std::string methodFunctionName;
        if (!classdefResolveMethodFunction(currentClass, "numArgumentsFromSubscript",
                methodFunctionName, eval->getClassdefAccessContext())) {
            return -1;
        }
        FunctionDef* methodDef = nullptr;
        if (!eval->lookupFunction(methodFunctionName, methodDef) || methodDef == nullptr) {
            return -1;
        }
        ArrayOfVector argIn;
        argIn.push_back(r);
        argIn.push_back(substruct);
        argIn.push_back(contextValue);
        ArrayOfVector rv = methodDef->evaluateFunction(eval, argIn, 1);
        if (rv.empty() || !rv[0].isScalar()) {
            return -1;
        }
        double n = rv[0].asDoubleScalar();
        if (!std::isfinite(n) || n < 0) {
            return -1;
        }
        return static_cast<int>(n);
    }
    //=============================================================================
    template <typename Operation>
    ArrayOf
    profiledArrayOperation(Evaluator* eval, const std::string& name, Operation operation)
    {
        Profiler* profiler = Profiler::getInstance();
        if (!profiler->isOn()) {
            return operation();
        }
        uint64 tic = profiler->tic();
        ArrayOf result = operation();
        if (tic != 0U && !name.empty()) {
            internalProfileFunction stack = computeProfileStack(
                eval, name, utf8_to_wstring(eval->callstack.getLastContext()));
            profiler->toc(tic, stack);
        }
        return result;
    }
    //=============================================================================
    template <typename Operation>
    void
    profiledVoidOperation(Evaluator* eval, const std::string& name, Operation operation)
    {
        Profiler* profiler = Profiler::getInstance();
        if (!profiler->isOn()) {
            operation();
            return;
        }
        uint64 tic = profiler->tic();
        operation();
        if (tic != 0U && !name.empty()) {
            internalProfileFunction stack = computeProfileStack(
                eval, name, utf8_to_wstring(eval->callstack.getLastContext()));
            profiler->toc(tic, stack);
        }
    }
    //=============================================================================
    std::string
    profileName(OpCode op)
    {
        switch (op) {
        case OpCode::OP_PLUS:
            return PLUS_OPERATOR_STR;
        case OpCode::OP_MINUS:
            return MINUS_OPERATOR_STR;
        case OpCode::OP_MTIMES:
            return MTIMES_OPERATOR_STR;
        case OpCode::OP_TIMES:
            return TIMES_OPERATOR_STR;
        case OpCode::OP_MRDIV:
            return MRDIVIDE_OPERATOR_STR;
        case OpCode::OP_RDIV:
            return RDIVIDE_OPERATOR_STR;
        case OpCode::OP_MLDIV:
            return MLDIVIDE_OPERATOR_STR;
        case OpCode::OP_LDIV:
            return LDIVIDE_OPERATOR_STR;
        case OpCode::OP_MPOWER:
            return MPOWER_OPERATOR_STR;
        case OpCode::OP_POWER:
            return POWER_OPERATOR_STR;
        case OpCode::OP_COLON_UNIT:
        case OpCode::OP_COLON:
            return COLON_OPERATOR_STR;
        case OpCode::OP_UPLUS:
            return UPLUS_OPERATOR_STR;
        case OpCode::OP_UMINUS:
            return UMINUS_OPERATOR_STR;
        case OpCode::OP_NOT:
            return NOT_OPERATOR_STR;
        case OpCode::OP_TRANSPOSE:
            return CTRANSPOSE_OPERATOR_STR;
        case OpCode::OP_DOT_TRANSPOSE:
            return TRANSPOSE_OPERATOR_STR;
        case OpCode::OP_LT:
            return LT_OPERATOR_STR;
        case OpCode::OP_LEQ:
            return LE_OPERATOR_STR;
        case OpCode::OP_GT:
            return GT_OPERATOR_STR;
        case OpCode::OP_GEQ:
            return GE_OPERATOR_STR;
        case OpCode::OP_EQ:
            return EQ_OPERATOR_STR;
        case OpCode::OP_NEQ:
            return NE_OPERATOR_STR;
        case OpCode::OP_AND:
            return AND_OPERATOR_STR;
        case OpCode::OP_OR:
            return OR_OPERATOR_STR;
        default:
            return {};
        }
    }
    //=============================================================================
    ArrayOf
    binaryOperator(Evaluator* eval, OpCode op, const ArrayOf& a, const ArrayOf& b)
    {
        if (!Profiler::getInstance()->isOn() && a.isDoubleType(true) && b.isDoubleType(true)
            && a.elementCount() == 1 && b.elementCount() == 1) {
            double av = *static_cast<const double*>(a.dataPointer());
            double bv = *static_cast<const double*>(b.dataPointer());
            switch (op) {
            case OpCode::OP_PLUS:
                return ArrayOf::doubleScalar(av + bv);
            case OpCode::OP_MINUS:
                return ArrayOf::doubleScalar(av - bv);
            case OpCode::OP_TIMES:
            case OpCode::OP_MTIMES:
                return ArrayOf::doubleScalar(av * bv);
            case OpCode::OP_RDIV:
            case OpCode::OP_MRDIV:
                return ArrayOf::doubleScalar(av / bv);
            case OpCode::OP_LDIV:
            case OpCode::OP_MLDIV:
                return ArrayOf::doubleScalar(bv / av);
            case OpCode::OP_LT:
                return ArrayOf::logicalScalar(av < bv);
            case OpCode::OP_LEQ:
                return ArrayOf::logicalScalar(av <= bv);
            case OpCode::OP_GT:
                return ArrayOf::logicalScalar(av > bv);
            case OpCode::OP_GEQ:
                return ArrayOf::logicalScalar(av >= bv);
            case OpCode::OP_EQ:
                return ArrayOf::logicalScalar(av == bv);
            case OpCode::OP_NEQ:
                return ArrayOf::logicalScalar(av != bv);
            case OpCode::OP_AND:
                return ArrayOf::logicalScalar((av != 0.0) && (bv != 0.0));
            case OpCode::OP_OR:
                return ArrayOf::logicalScalar((av != 0.0) || (bv != 0.0));
            default:
                break;
            }
        }
        // Scalar fast path for complex (or mixed real/complex) double scalars, which the real-only
        // guard above skips. Computes +, -, *, / and (in)equality on std::complex<double> with the
        // general path's narrowing (imag == 0 -> real double) and zero-divisor semantics.
        if (!Profiler::getInstance()->isOn() && a.isDoubleType(false) && b.isDoubleType(false)
            && a.elementCount() == 1 && b.elementCount() == 1 && (a.isComplex() || b.isComplex())) {
            auto readScalarComplex = [](const ArrayOf& x) -> std::complex<double> {
                const double* ptr = static_cast<const double*>(x.dataPointer());
                if (x.isComplex()) {
                    return std::complex<double>(ptr[0], ptr[1]);
                }
                return std::complex<double>(ptr[0], 0.0);
            };
            // Divide: a real divisor scales both parts (bit-identical to the complex/real kernel);
            // a zero divisor gives NaN+NaNi (like normalizeComplexZeroDivisors).
            auto scalarComplexDivide = [](std::complex<double> num, std::complex<double> den,
                                           bool denIsReal) -> std::complex<double> {
                if (denIsReal) {
                    const double c = den.real();
                    if (std::isfinite(c) && c != 0.0 && std::isfinite(num.real())
                        && std::isfinite(num.imag())) {
                        return std::complex<double>(num.real() / c, num.imag() / c);
                    }
                }
                if (den.real() == 0.0 && den.imag() == 0.0) {
                    const double nan = std::numeric_limits<double>::quiet_NaN();
                    return std::complex<double>(nan, nan);
                }
                return num / den;
            };
            const std::complex<double> av = readScalarComplex(a);
            const std::complex<double> bv = readScalarComplex(b);
            std::complex<double> result;
            bool handled = true;
            switch (op) {
            case OpCode::OP_PLUS:
                result = av + bv;
                break;
            case OpCode::OP_MINUS:
                result = av - bv;
                break;
            case OpCode::OP_TIMES:
            case OpCode::OP_MTIMES:
                result = av * bv;
                break;
            case OpCode::OP_RDIV:
            case OpCode::OP_MRDIV:
                result = scalarComplexDivide(av, bv, !b.isComplex());
                break;
            case OpCode::OP_LDIV:
            case OpCode::OP_MLDIV:
                result = scalarComplexDivide(bv, av, !a.isComplex());
                break;
            case OpCode::OP_EQ:
                return ArrayOf::logicalScalar(av == bv);
            case OpCode::OP_NEQ:
                return ArrayOf::logicalScalar(av != bv);
            default:
                handled = false;
                break;
            }
            if (handled) {
                if (result.imag() == 0.0) {
                    return ArrayOf::doubleScalar(result.real());
                }
                return ArrayOf::dsingleComplexScalar(result.real(), result.imag());
            }
        }
        return profiledArrayOperation(eval, profileName(op), [&]() {
            ArrayOfVector args;
            args.reserve(2);
            args.push_back(a);
            args.push_back(b);
            switch (op) {
            case OpCode::OP_PLUS:
                return eval->plusOperator(args);
            case OpCode::OP_MINUS:
                return eval->minusOperator(args);
            case OpCode::OP_MTIMES:
                return eval->mtimesOperator(args);
            case OpCode::OP_TIMES:
                return eval->timesOperator(args);
            case OpCode::OP_MRDIV:
                return eval->rightDivideOperator(args);
            case OpCode::OP_RDIV:
                return eval->dotRightDivideOperator(args);
            case OpCode::OP_MLDIV:
                return eval->leftDivideOperator(args);
            case OpCode::OP_LDIV:
                return eval->dotLeftDivideOperator(args);
            case OpCode::OP_MPOWER:
                return eval->mpowerOperator(args);
            case OpCode::OP_POWER:
                return eval->powerOperator(args);
            case OpCode::OP_LT:
                return eval->ltOperator(args);
            case OpCode::OP_LEQ:
                return eval->leOperator(args);
            case OpCode::OP_GT:
                return eval->gtOperator(args);
            case OpCode::OP_GEQ:
                return eval->geOperator(args);
            case OpCode::OP_EQ:
                return eval->eqOperator(args);
            case OpCode::OP_NEQ:
                return eval->neOperator(args);
            case OpCode::OP_AND:
                return eval->andOperator(args);
            case OpCode::OP_OR:
                return eval->orOperator(args);
            case OpCode::OP_COLON_UNIT:
                return eval->colonUnitOperator(a, b);
            default:
                Error(L"Nelson:bytecode:unsupportedBinaryOperator",
                    L"BytecodeVM: unsupported binary operator.");
            }
            return ArrayOf::empty();
        });
    }
    //=============================================================================
    bool
    tryScalarDoubleComparison(OpCode op, const ArrayOf& a, const ArrayOf& b, bool& result)
    {
        if (Profiler::getInstance()->isOn() || !a.isDoubleType(true) || !b.isDoubleType(true)
            || a.elementCount() != 1 || b.elementCount() != 1) {
            return false;
        }
        double av = *static_cast<const double*>(a.dataPointer());
        double bv = *static_cast<const double*>(b.dataPointer());
        switch (op) {
        case OpCode::OP_LT:
            result = av < bv;
            return true;
        case OpCode::OP_LEQ:
            result = av <= bv;
            return true;
        case OpCode::OP_GT:
            result = av > bv;
            return true;
        case OpCode::OP_GEQ:
            result = av >= bv;
            return true;
        case OpCode::OP_EQ:
            result = av == bv;
            return true;
        case OpCode::OP_NEQ:
            result = av != bv;
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    bool
    isScalarDoubleZeroConstant(const BytecodeChunk& chunk, uint16_t constantIndex)
    {
        const ConstantValue& value = chunk.constants.get(constantIndex);
        const auto* arrayValue = std::get_if<ArrayOf>(&value);
        if (arrayValue == nullptr || !arrayValue->isDoubleType(true)
            || arrayValue->elementCount() != 1) {
            return false;
        }
        return *static_cast<const double*>(arrayValue->dataPointer()) == 0.0;
    }
    //=============================================================================
    bool
    tryCompareScalarDoubleWithZero(OpCode op, double value, bool& result)
    {
        switch (op) {
        case OpCode::OP_LT:
            result = value < 0.0;
            return true;
        case OpCode::OP_LEQ:
            result = value <= 0.0;
            return true;
        case OpCode::OP_GT:
            result = value > 0.0;
            return true;
        case OpCode::OP_GEQ:
            result = value >= 0.0;
            return true;
        case OpCode::OP_EQ:
            result = value == 0.0;
            return true;
        case OpCode::OP_NEQ:
            result = value != 0.0;
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    ArrayOf
    ternaryOperator(
        Evaluator* eval, OpCode op, const ArrayOf& a, const ArrayOf& b, const ArrayOf& c)
    {
        return profiledArrayOperation(eval, profileName(op), [&]() {
            switch (op) {
            case OpCode::OP_COLON:
                return eval->colonOperator(a, b, c);
            default:
                Error(L"Nelson:bytecode:unsupportedTernaryOperator",
                    L"BytecodeVM: unsupported ternary operator.");
            }
            return ArrayOf::empty();
        });
    }
    //=============================================================================
    ArrayOf
    unaryOperator(Evaluator* eval, OpCode op, const ArrayOf& a)
    {
        if (!Profiler::getInstance()->isOn() && a.isDoubleType(true) && a.elementCount() == 1) {
            double av = a.asDoubleScalar();
            switch (op) {
            case OpCode::OP_UPLUS:
            case OpCode::OP_TRANSPOSE:
            case OpCode::OP_DOT_TRANSPOSE:
                return ArrayOf::doubleScalar(av);
            case OpCode::OP_UMINUS:
                return ArrayOf::doubleScalar(-av);
            case OpCode::OP_NOT:
                return ArrayOf::logicalScalar(!(av != 0.0));
            default:
                break;
            }
        }
        return profiledArrayOperation(eval, profileName(op), [&]() {
            switch (op) {
            case OpCode::OP_UPLUS:
                return eval->uplusOperator(a);
            case OpCode::OP_UMINUS:
                return eval->uminusOperator(a);
            case OpCode::OP_NOT:
                return eval->notOperator(a);
            case OpCode::OP_TRANSPOSE:
                return eval->complexTransposeOperator(a);
            case OpCode::OP_DOT_TRANSPOSE:
                return eval->transposeOperator(a);
            default:
                Error(L"Nelson:bytecode:unsupportedUnaryOperator",
                    L"BytecodeVM: unsupported unary operator.");
            }
            return ArrayOf::empty();
        });
    }
    //=============================================================================
    ArrayOf
    loopValueAt(GenericLoopState& state)
    {
        if (state.isRowVector) {
            switch (state.indexSet.dataClass()) {
            case NLS_LOGICAL:
            case NLS_UINT8:
            case NLS_INT8:
            case NLS_UINT16:
            case NLS_INT16:
            case NLS_UINT32:
            case NLS_INT32:
            case NLS_UINT64:
            case NLS_INT64:
            case NLS_SINGLE:
            case NLS_DOUBLE:
            case NLS_SCOMPLEX:
            case NLS_DCOMPLEX:
            case NLS_CHAR:
                return state.indexSet.valueAtIndex(static_cast<uint64>(state.current));
            default: {
                ArrayOf index = ArrayOf::doubleScalar(static_cast<double>(state.current + 1));
                return state.indexSet.vectorSubset(index);
            }
            }
        }

        indexType rows = state.indexSet.rows();
        ArrayOfVector indices;
        indices.reserve(2);
        indices.push_back(ArrayOf::integerRange(1, 1, rows, false));
        indices.push_back(ArrayOf::doubleScalar(static_cast<double>(state.current + 1)));
        return state.indexSet.nDimSubset(indices);
    }
    //=============================================================================
    void
    initGenericLoop(VMCallFrame& frame, uint16_t loopSlot, uint16_t varSlot, ArrayOf indexSet,
        uint32_t& pc, uint16_t emptyTarget)
    {
        if (loopSlot >= frame.loopStates.size() || varSlot >= frame.locals.size()) {
            Error(L"Nelson:bytecode:forLoopOperandOutOfRange",
                L"BytecodeVM: for-loop operand out of range.");
        }
        GenericLoopState& state = frame.loopStates[loopSlot];
        state.indexSet = indexSet;
        state.isRowVector = state.indexSet.isRowVector();
        state.current = 0;
        if (state.indexSet.elementCount() == 0) {
            state.count = 0;
        } else {
            state.count = state.isRowVector
                ? state.indexSet.elementCount()
                : (state.indexSet.isColumnVector() ? 1 : state.indexSet.columns());
        }
        if (state.count == 0) {
            pc = emptyTarget;
        } else {
            frame.locals[varSlot] = loopValueAt(state);
            markRuntimeAssigned(frame, varSlot);
        }
    }
    //=============================================================================
    void
    initGenericLoopGlobal(Context* context, const BytecodeChunk& chunk, VMCallFrame& frame,
        uint16_t loopSlot, uint16_t nameSlot, ArrayOf indexSet, uint32_t& pc, uint16_t emptyTarget)
    {
        if (loopSlot >= frame.loopStates.size() || nameSlot >= chunk.names.size()) {
            Error(L"Nelson:bytecode:forLoopOperandOutOfRange",
                L"BytecodeVM: for-loop operand out of range.");
        }
        GenericLoopState& state = frame.loopStates[loopSlot];
        state.indexSet = indexSet;
        state.isRowVector = state.indexSet.isRowVector();
        state.current = 0;
        if (state.indexSet.elementCount() == 0) {
            state.count = 0;
        } else {
            state.count = state.isRowVector
                ? state.indexSet.elementCount()
                : (state.indexSet.isColumnVector() ? 1 : state.indexSet.columns());
        }
        if (state.count == 0) {
            pc = emptyTarget;
        } else {
            context->insertVariable(chunk.names[nameSlot], loopValueAt(state));
        }
    }
    //=============================================================================
    void
    stepGenericLoop(
        VMCallFrame& frame, uint16_t loopSlot, uint16_t varSlot, uint32_t& pc, uint16_t bodyTarget)
    {
        if (loopSlot >= frame.loopStates.size() || varSlot >= frame.locals.size()) {
            Error(L"Nelson:bytecode:forLoopOperandOutOfRange",
                L"BytecodeVM: for-loop operand out of range.");
        }
        GenericLoopState& state = frame.loopStates[loopSlot];
        ++state.current;
        if (state.current < state.count) {
            frame.locals[varSlot] = loopValueAt(state);
            markRuntimeAssigned(frame, varSlot);
            pc = bodyTarget;
        }
    }
    //=============================================================================
    void
    stepGenericLoopGlobal(Context* context, const BytecodeChunk& chunk, VMCallFrame& frame,
        uint16_t loopSlot, uint16_t nameSlot, uint32_t& pc, uint16_t bodyTarget)
    {
        if (loopSlot >= frame.loopStates.size() || nameSlot >= chunk.names.size()) {
            Error(L"Nelson:bytecode:forLoopOperandOutOfRange",
                L"BytecodeVM: for-loop operand out of range.");
        }
        GenericLoopState& state = frame.loopStates[loopSlot];
        ++state.current;
        if (state.current < state.count) {
            context->insertVariable(chunk.names[nameSlot], loopValueAt(state));
            pc = bodyTarget;
        }
    }
    //=============================================================================
    void
    setLocalDouble(std::vector<ArrayOf>& locals, uint16_t slot, double value)
    {
        if (slot >= locals.size()) {
            Error(L"Nelson:bytecode:localOperandOutOfRange",
                L"BytecodeVM: local operand out of range.");
        }
        ArrayOf& target = locals[slot];
        if (!target.isDoubleType(true) || target.elementCount() != 1) {
            target = ArrayOf::doubleScalar(value);
            return;
        }
        *static_cast<double*>(target.readWriteDataPointer()) = value;
    }
    //=============================================================================
    void
    alignStructFieldsForSubsetAssign(ArrayOf& base, const ArrayOf& value)
    {
        if (!base.isStruct() || !value.isStruct()) {
            return;
        }
        stringVector baseFields = base.fieldNames();
        stringVector valueFields = value.fieldNames();
        stringVector mergedFields = baseFields;
        for (const std::string& field : valueFields) {
            if (std::find(baseFields.begin(), baseFields.end(), field) != baseFields.end()) {
                continue;
            }
            mergedFields.push_back(field);
        }
        if (mergedFields.size() != baseFields.size()) {
            base.promoteType(NLS_STRUCT_ARRAY, mergedFields);
        }
    }
    //=============================================================================
    bool
    assignStructVectorSubset(ArrayOf& base, ArrayOf index, ArrayOf value)
    {
        if (!base.isStruct() || !value.isStruct()) {
            return false;
        }

        alignStructFieldsForSubsetAssign(base, value);
        value.promoteType(NLS_STRUCT_ARRAY, base.fieldNames());
        base.setVectorSubset(index, value);
        return true;
    }
    //=============================================================================
    ArrayOf
    assignParensToValue(Evaluator* eval, ArrayOf base, ArrayOfVector indices, ArrayOf value)
    {
        if (indices.empty()) {
            Error(L"Nelson:interpreter:indexExpressionExpected", L"Index expression expected.");
        }
        if (base.isClassType() || base.isHandle()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back("()");
            subsindices.push_back(classIndexCell(indices));
            bool haveFunction = false;
            ArrayOf assigned
                = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
            if (haveFunction) {
                return assigned;
            }
            if (!base.isHandle() && !classdefLoadClass(base.classType())) {
                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
            }
        }
        if (base.isFunctionHandle()) {
            Error(L"Nelson:interpreter:invalidFieldAssignment",
                L"Cannot apply A.field_name = B to non struct-array object A.");
        }
        resolveObjectIndices(eval, indices);
        if (tryAssignStringScalarNDimDirect(base, indices, value)) {
            return base;
        }
        alignStructFieldsForSubsetAssign(base, value);
        normalizeParensAssignmentValueForColon(indices, value);
        if (tryAssignVectorColon(base, indices, value)) {
            return base;
        }
        if (tryAssignEmptyCellColumn(base, indices, value)) {
            return base;
        }
        expandColonParensAssignmentIndices(base, indices, value);
        normalizeParensAssignmentValueForTarget(indices, value);
        expandClassdefAssignmentBaseWithDefaults(eval, base, indices, value);
        if (indices.size() == 1) {
            if (!assignStructVectorSubset(base, indices[0], value)) {
                base.setVectorSubset(indices[0], value);
            }
        } else if (!tryAssignRowColonSlice(base, indices, value)) {
            base.setNDimSubset(indices, value);
        }
        return base;
    }
    //=============================================================================
    ArrayOf
    assignBracesToValue(Evaluator* eval, ArrayOf base, ArrayOfVector indices, const ArrayOf& value)
    {
        if (indices.empty()) {
            Error(L"Nelson:interpreter:indexExpressionExpected", L"Index expression expected.");
        }
        ArrayOfVector values;
        values.push_back(value);
        if (base.isClassType() || base.isHandle()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back("{}");
            subsindices.push_back(classIndexCell(indices));
            bool haveFunction = false;
            ArrayOf assigned
                = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
            if (haveFunction) {
                return assigned;
            }
            if (base.isClassType()) {
                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
            }
        }
        checkBracesAssignTarget(base);
        resolveObjectIndices(eval, indices);
        expandColonContentAssignmentIndices(base, indices);
        try {
            if (indices.size() == 1) {
                base.setVectorContentsAsList(indices[0], values);
            } else {
                base.setNDimContentsAsList(indices, values);
            }
        } catch (const NelsonException& e) {
            if (e.message()
                    == Nelson::resolveMessageTemplate(
                        L"Nelson:interpreter:illegalZeroNegativeIndex",
                        L"Illegal zero or negative index")
                || e.message()
                    == resolveMessageTemplate(L"Nelson:interpreter:cannotConvertStringData",
                        L"Cannot convert string data types to indices.")) {
                Error(L"Nelson:interpreter:notEnoughRhsValues",
                    L"Not enough right hand side values to satisfy left hand side expression.");
            }
            throw;
        }
        return base;
    }
    //=============================================================================
    ArrayOf
    assignDotToValue(Evaluator* eval, ArrayOf base, const std::string& field, const ArrayOf& value)
    {
        ArrayOfVector values;
        values.push_back(value);
        if (base.isClassType()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back(".");
            subsindices.push_back(ArrayOf::characterArray(field));
            bool haveFunction = false;
            ArrayOf assigned
                = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
            if (!haveFunction) {
                Error(L"Nelson:interpreter:invalidFieldAssignment",
                    L"Cannot apply A.field_name = B to non struct-array object A.");
            }
            return assigned;
        }
        if (base.isHandle() || base.isGraphicsObject()) {
            eval->bytecodeSetHandle(base, field, values);
            return base;
        }
        checkDotAssignTarget(base);
        if (tryAssignStructScalarFieldDirect(base, field, value)) {
            return base;
        }
        if (base.isEmpty() || (base.isStruct() && base.isScalar())) {
            ArrayOf fieldValue = value;
            base.setField(field, fieldValue);
            return base;
        }
        base.setFieldAsList(field, values);
        return base;
    }
    //=============================================================================
    // Distribute the N values of `values` across the field `field` of an N-element struct array
    // `base`, as in `[s.field] = rhs`. N matches numel(base). Class/handle objects reuse the
    // single-value dot-assignment path (scalar objects only).
    ArrayOf
    assignFieldMultiValue(
        Evaluator* eval, ArrayOf base, const std::string& field, ArrayOfVector& values)
    {
        // numel(base) == 1: identical to a single-target `s.field = value`.
        if (values.size() == 1) {
            return assignDotToValue(eval, base, field, values[0]);
        }
        if (base.isClassType() || base.isHandle() || base.isGraphicsObject()) {
            Error(L"Nelson:interpreter:unsupportedFieldAssignment",
                L"Field assignment to a non-scalar class or handle array with [obj.field] = ... is "
                L"not supported.");
        }
        checkDotAssignTarget(base);
        base.setFieldAsList(field, values);
        return base;
    }
    //=============================================================================
    void
    assignConstantParensIndex(
        Evaluator* eval, VMCallFrame& frame, uint16_t slot, ArrayOf index, ArrayOf value)
    {
        if (slot >= frame.locals.size()) {
            Error(L"Nelson:bytecode:localSlotOutOfRange", L"BytecodeVM: local slot out of range.");
        }
        ArrayOf& base = frame.locals[slot];
        if (tryAssignDoubleScalarLinearDirect(base, index, value)
            || tryAssignStringScalarLinearDirect(base, index, value)) {
            markRuntimeAssigned(frame, slot);
            return;
        }
        ArrayOfVector indices;
        indices.push_back(index);
        if (base.isClassType() || base.isHandle()) {
            stringVector subtypes;
            ArrayOfVector subsindices;
            subtypes.push_back("()");
            subsindices.push_back(classIndexCell(indices));
            bool haveFunction = false;
            ArrayOf assigned
                = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
            if (!haveFunction) {
                if (base.isHandle() || classdefLoadClass(base.classType())) {
                    alignStructFieldsForSubsetAssign(base, value);
                    if (!assignStructVectorSubset(base, index, value)) {
                        base.setVectorSubset(index, value);
                    }
                    markRuntimeAssigned(frame, slot);
                    return;
                }
                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
            }
            base = assigned;
            markRuntimeAssigned(frame, slot);
            return;
        }
        alignStructFieldsForSubsetAssign(base, value);
        if (!assignStructVectorSubset(base, index, value)) {
            base.setVectorSubset(index, value);
        }
        markRuntimeAssigned(frame, slot);
    }
    //=============================================================================
    void
    assignConstantStringScalarIndex(
        Evaluator* eval, VMCallFrame& frame, const Instruction& ins, const ArrayOf& element)
    {
        if (ins.A >= frame.locals.size()) {
            Error(L"Nelson:bytecode:localSlotOutOfRange", L"BytecodeVM: local slot out of range.");
        }
        ArrayOf& base = frame.locals[ins.A];
        if (tryAssignStringScalarElementAtOrdinal(base, ins.B, element)) {
            markRuntimeAssigned(frame, ins.A);
            return;
        }
        ArrayOf index = ArrayOf::doubleScalar(static_cast<double>(ins.B));
        ArrayOf value = (ins.flags & INST_FLAG_VALUE_IS_STRING) != 0
            ? ArrayOf::stringArray(element.asUtf8String())
            : element;
        assignConstantParensIndex(eval, frame, ins.A, index, value);
    }
    //=============================================================================
    void
    applyImportDeclarations(Evaluator* eval, const BytecodeChunk& chunk)
    {
        if (!chunk.hasDeclareImport || eval == nullptr || eval->getContext() == nullptr) {
            return;
        }
        Context* context = eval->getContext();
        for (const Instruction& ins : chunk.code) {
            if (ins.op != OpCode::DECLARE_IMPORT) {
                continue;
            }
            if (ins.A >= chunk.names.size()) {
                Error(L"Nelson:bytecode:nameIndexOutOfRange",
                    L"BytecodeVM: name index out of range.");
            }
            context->addImport(chunk.names[ins.A]);
        }
    }
    //=============================================================================
    void
    runChunk(Evaluator* eval, VMCallFrame& frame)
    {
        const BytecodeChunk& chunk = *frame.chunk;
        std::vector<ArrayOf>& stack = frame.valueStack;
        stack.clear();
        stack.reserve(chunk.maxStack == 0 ? 8 : chunk.maxStack);
        uint32_t pc = frame.startPC;
        Context* context = eval->getContext();

        auto pop = [&]() -> ArrayOf { return popStackValue(stack); };
        auto popVector
            = [&](uint16_t count) -> ArrayOfVector { return popStackValues(stack, count); };

        auto syncScriptLocals = [&]() {
            if (!chunk.isScript) {
                return;
            }
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                const std::string& name = chunk.localNames[k];
                if (!name.empty() && isRuntimeAssignedLocalSlot(frame, k)) {
                    context->insertVariableLocally(name, frame.locals[k]);
                }
            }
            // The scope snapshot co-owns each local; record it so a following
            // in-place indexed assignment drops the stale snapshot before
            // mutating, keeping per-element stores in a script loop O(1).
            frame.localsSyncedToContext = true;
            // The context copy of a reassigned local was the last holder of
            // its previous value: finalize it before the callee runs.
            FinalizePendingHandles(eval);
        };

        auto syncAssignedLocalsToContext = [&]() {
            if (chunk.isScript) {
                syncScriptLocals();
                return;
            }
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                const std::string& name = chunk.localNames[k];
                if (!name.empty() && isRuntimeAssignedLocalSlot(frame, k)) {
                    context->insertVariableLocally(name, frame.locals[k]);
                }
            }
            // The snapshot co-owns each local; record it so a following in-place
            // indexed assignment drops the stale snapshot before mutating,
            // instead of COW-copying the whole array on every call.
            frame.localsSyncedToContext = true;
            FinalizePendingHandles(eval);
        };

        // A local reassigned after a sync leaves a stale copy of its previous value in the context
        // scope. For a handle object or a function handle (whose definition owns the workspace it
        // captured) that copy would delay the destructor, so it is refreshed right away. Only
        // reached when the previous value is one of those.
        auto refreshStaleContextHandleCopy = [&](uint16_t slot, const ArrayOf& newValue) {
            if (!frame.hasContextScope || slot >= chunk.localNames.size()
                || !isRuntimeAssignedLocalSlot(frame, slot)) {
                return;
            }
            const std::string& name = chunk.localNames[slot];
            if (name.empty()) {
                return;
            }
            ArrayOf current;
            if (context->lookupVariableLocally(name, current)
                && (current.isHandle() || current.isFunctionHandle())) {
                context->insertVariableLocally(name, newValue);
            }
        };

        auto refreshLocalsFromContext = [&]() {
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                const std::string& name = chunk.localNames[k];
                if (!name.empty()) {
                    ArrayOf value;
                    if (context->lookupVariable(name, value)) {
                        frame.locals[k] = value;
                        markRuntimeAssigned(frame, k);
                    }
                }
            }
        };

        auto refreshLocalsFromContextClearingMissing = [&]() {
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                const std::string& name = chunk.localNames[k];
                if (name.empty()) {
                    continue;
                }
                ArrayOf value;
                if (context->lookupVariable(name, value)) {
                    frame.locals[k] = value;
                    markRuntimeAssigned(frame, k);
                } else {
                    frame.locals[k] = ArrayOf::empty();
                    markRuntimeUnassigned(frame, k);
                }
            }
        };

        auto refreshAfterVariableClear = [&](size_t previousEpoch) {
            if (context->getVariablesClearedEpoch() != previousEpoch) {
                refreshLocalsFromContextClearingMissing();
            }
        };

        uint32_t statementBoundaryCounter = 0;
        NelsonConfiguration* nlsConfig = NelsonConfiguration::getInstance();
        WebDesktopBridge* webEventPumpBridge = WebDesktopBridge::getInstance();
        auto processStatementBoundaryQueues = [&]() {
            const bool hasOpenFigureWindows = nlsConfig->hasOpenFigureWindows();
            const bool coarseBoundary = (++statementBoundaryCounter & 63U) == 0;
            // Keep figure windows responsive while a script runs. With a Qt figure window open,
            // service the GUI at every statement boundary (ProcessEvents self-throttles to ~1ms).
            // Otherwise keep the coarse cadence: the only cost is one relaxed atomic load.
            if (hasOpenFigureWindows) {
                if (eval->haveEventsLoop()) {
                    Nelson::processGuiEvents(false);
                }
            } else if (coarseBoundary) {
                if (eval->haveEventsLoop()) {
                    Nelson::processGuiEvents(false);
                }
            }
            // nelson-adv-cli --webview: service the native figure windows on this
            // (interpreter/main) thread. No-op unless the figures-only web desktop
            // registered the pump hook.
            if (webEventPumpBridge->hasWebEventPumpHook()
                && (hasOpenFigureWindows || coarseBoundary)) {
                webEventPumpBridge->pumpWebEvents();
            }
            if (!eval->commandQueue.isEmpty()) {
                syncAssignedLocalsToContext();
                // Drain the queued cooperative commands instead of one per statement boundary:
                // running one at a time serializes a burst of figure key/mouse events and is felt
                // as input lag. The cap keeps a runaway producer from starving the statement being
                // executed.
                constexpr int maxQueuedCommandsPerBoundary = 64;
                std::wstring command;
                int drained = 0;
                while (drained < maxQueuedCommandsPerBoundary && eval->commandQueue.get(command)) {
                    eval->evaluateString(command);
                    ++drained;
                }
                refreshLocalsFromContext();
            }
            if (!MainThreadTaskQueue::getInstance()->isEmpty()) {
                // Cross-thread tasks may run Nelson code in the base scope;
                // guard locals exactly like the commandQueue drain above.
                syncAssignedLocalsToContext();
                MainThreadTaskQueue::getInstance()->processTasks(eval);
                refreshLocalsFromContext();
            }
        };

        auto executeLoadLocal = [&](const Instruction& loadIns) {
            if (loadIns.A >= frame.locals.size()) {
                Error(L"Nelson:bytecode:localSlotOutOfRange",
                    L"BytecodeVM: local slot out of range.");
            }
            if (!isRuntimeAssignedLocalSlot(frame, loadIns.A)) {
                if (loadIns.A < chunk.localNames.size() && !chunk.localNames[loadIns.A].empty()) {
                    ArrayOf value;
                    if (context->lookupVariable(chunk.localNames[loadIns.A], value)) {
                        stack.push_back(value);
                        return;
                    }
                    if ((loadIns.flags & INST_FLAG_ALLOW_UNDEFINED) == 0
                        && tryLoadImplicitArgumentCount(
                            context, chunk.localNames[loadIns.A], value)) {
                        stack.push_back(value);
                        return;
                    }
                    if ((loadIns.flags & INST_FLAG_ALLOW_UNDEFINED) == 0) {
                        raiseUndefinedVariable(context, chunk, chunk.localNames[loadIns.A]);
                    }
                }
                stack.push_back(ArrayOf::empty());
                return;
            }
            stack.push_back(frame.locals[loadIns.A]);
        };
        auto tryGetAssignedLocalByName = [&](const std::string& name, ArrayOf& value) -> bool {
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                if (chunk.localNames[k] == name && isRuntimeAssignedLocalSlot(frame, k)) {
                    value = frame.locals[k];
                    return true;
                }
            }
            return false;
        };

        // Indexed assignments mutate frame.locals[slot] in place, but until the slot is assigned
        // the variable may still live in the context (a script extends its caller's workspace).
        // Seed the slot from the context so the mutation grows the existing value. An absent
        // variable stays unassigned and keeps its create-from-empty behavior.
        auto seedAssignBaseFromContext = [&](uint16_t slot) {
            if (slot >= frame.locals.size()) {
                return;
            }
            if (isRuntimeAssignedLocalSlot(frame, slot)) {
                // The base already lives in this frame. A snapshot of it in the scope co-owns the
                // data and would make the following in-place indexed assignment COW-copy the whole
                // array. The snapshot is re-created before the next callee call, so dropping it is
                // safe.
                if (frame.localsSyncedToContext && frame.hasContextScope
                    && slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                    context->removeVariableLocally(chunk.localNames[slot]);
                }
                return;
            }
            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()
                && context->lookupVariable(chunk.localNames[slot], frame.locals[slot])) {
                markRuntimeAssigned(frame, slot);
            }
        };

        // The name-to-slot mapping only depends on the chunk, so the cache is
        // pooled with the frame buffer and survives across calls.
        std::vector<int>& namedLocalSlotCache = frame.namedLocalSlotCache;
        if (namedLocalSlotCache.size() != chunk.names.size()) {
            namedLocalSlotCache.assign(chunk.names.size(), -2);
        }
        auto localSlotForNameIndex = [&](uint16_t nameIndex) -> int {
            if (nameIndex >= namedLocalSlotCache.size()) {
                return -1;
            }
            int& cachedSlot = namedLocalSlotCache[nameIndex];
            if (cachedSlot != -2) {
                return cachedSlot;
            }
            cachedSlot = -1;
            const std::string& name = chunk.names[nameIndex];
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                if (chunk.localNames[k] == name) {
                    cachedSlot = static_cast<int>(k);
                    break;
                }
            }
            return cachedSlot;
        };

        auto tryGetAssignedLocalByNameIndex = [&](uint16_t nameIndex, ArrayOf& value) -> bool {
            int slot = localSlotForNameIndex(nameIndex);
            if (slot < 0 || !isRuntimeAssignedLocalSlot(frame, static_cast<size_t>(slot))) {
                return false;
            }
            value = frame.locals[static_cast<size_t>(slot)];
            return true;
        };

        // Function resolutions are pooled with the frame buffer and reused as
        // long as the function tables generation does not change. Freshness is
        // rechecked per lookup: a callee can change the tables mid-run.
        std::vector<FunctionDef*>& namedScalarBuiltinCache = frame.namedScalarBuiltinCache;
        std::vector<uint8_t>& namedScalarBuiltinCacheState = frame.namedScalarBuiltinCacheState;
        std::vector<FunctionDef*>& namedFunctionCache = frame.namedFunctionCache;
        std::vector<uint8_t>& namedFunctionCacheState = frame.namedFunctionCacheState;
        auto ensureNamedFunctionCachesFresh = [&]() {
            const uint64 functionTablesGeneration = FunctionsInMemory::getInstance()->generation();
            if (namedScalarBuiltinCache.size() != chunk.names.size()
                || frame.namedBuiltinCacheGeneration != functionTablesGeneration) {
                namedScalarBuiltinCache.assign(chunk.names.size(), nullptr);
                namedScalarBuiltinCacheState.assign(chunk.names.size(), 0);
                namedFunctionCache.assign(chunk.names.size(), nullptr);
                namedFunctionCacheState.assign(chunk.names.size(), 0);
                frame.namedBuiltinCacheGeneration = functionTablesGeneration;
            }
        };
        ensureNamedFunctionCachesFresh();
        auto lookupCachedFunction
            = [&](uint16_t nameIndex, const std::string& name, FunctionDef*& fdef) -> bool {
            ensureNamedFunctionCachesFresh();
            if (nameIndex >= namedFunctionCacheState.size()) {
                return eval->lookupFunction(name, fdef);
            }
            uint8_t state = namedFunctionCacheState[nameIndex];
            if (state == 1) {
                fdef = namedFunctionCache[nameIndex];
                return true;
            }
            if (state == 2) {
                return false;
            }
            FunctionDef* resolved = nullptr;
            if (!eval->lookupFunction(name, resolved) || resolved == nullptr) {
                namedFunctionCacheState[nameIndex] = 2;
                return false;
            }
            namedFunctionCache[nameIndex] = resolved;
            namedFunctionCacheState[nameIndex] = 1;
            fdef = resolved;
            return true;
        };
        // A dotted classdef member (`Class.ConstantProperty`, enumeration member or static method)
        // compiled to CALL_NAMED binds to the generated member function. `clear classes`
        // unregisters that function, so reload the class as CALL_CLASSDEF_MEMBER does and look the
        // member up again.
        auto tryReloadStaleClassdefMemberFunction
            = [&](const std::string& name, FunctionDef*& fdef) -> bool {
            std::string className;
            std::string memberName;
            if (!splitClassdefMemberSpec(name, className, memberName)) {
                return false;
            }
            const std::string& callerClassName = eval->getClassdefAccessContext();
            std::string resolvedFunctionName;
            if (!classdefResolveConstantPropertyFunction(
                    className, memberName, resolvedFunctionName, callerClassName)
                && !classdefResolveEnumerationMemberFunction(
                    className, memberName, resolvedFunctionName)
                && !classdefResolveStaticMethodFunction(
                    className, memberName, resolvedFunctionName, callerClassName)) {
                return false;
            }
            return eval->lookupFunction(name, fdef) && fdef != nullptr;
        };
        // Scalar double constants only depend on the chunk.
        std::vector<double>& scalarDoubleConstantCache = frame.scalarDoubleConstantCache;
        std::vector<uint8_t>& scalarDoubleConstantCacheState = frame.scalarDoubleConstantCacheState;
        auto loadCachedScalarDoubleConstant = [&](uint16_t constantIndex, double& value) -> bool {
            if (constantIndex >= chunk.constants.size()) {
                return false;
            }
            if (scalarDoubleConstantCache.size() != chunk.constants.size()) {
                scalarDoubleConstantCache.assign(chunk.constants.size(), 0.);
                scalarDoubleConstantCacheState.assign(chunk.constants.size(), 0);
            }
            uint8_t state = scalarDoubleConstantCacheState[constantIndex];
            if (state == 1) {
                value = scalarDoubleConstantCache[constantIndex];
                return true;
            }
            if (state == 2) {
                return false;
            }
            if (!loadScalarDoubleConstant(chunk, constantIndex, value)) {
                scalarDoubleConstantCacheState[constantIndex] = 2;
                return false;
            }
            scalarDoubleConstantCache[constantIndex] = value;
            scalarDoubleConstantCacheState[constantIndex] = 1;
            return true;
        };
        auto isCachedScalarDoubleZeroConstant = [&](uint16_t constantIndex) -> bool {
            double value = 0.;
            return loadCachedScalarDoubleConstant(constantIndex, value) && value == 0.0;
        };
        auto lookupCachedScalarBuiltin
            = [&](uint16_t nameIndex, const std::string& name, FunctionDef*& fdef) -> bool {
            ensureNamedFunctionCachesFresh();
            if (nameIndex >= namedScalarBuiltinCacheState.size()) {
                return false;
            }
            uint8_t state = namedScalarBuiltinCacheState[nameIndex];
            if (state == 1) {
                fdef = namedScalarBuiltinCache[nameIndex];
                return fdef != nullptr;
            }
            if (state == 2) {
                return false;
            }
            FunctionDef* resolved = nullptr;
            if (!eval->lookupFunction(name, resolved) || resolved == nullptr
                || resolved->type() != NLS_BUILT_IN_FUNCTION || resolved->isOverload()) {
                namedScalarBuiltinCacheState[nameIndex] = 2;
                return false;
            }
            namedScalarBuiltinCache[nameIndex] = resolved;
            namedScalarBuiltinCacheState[nameIndex] = 1;
            fdef = resolved;
            return true;
        };

        std::function<bool(FunctionDef*)> needsCallerContextSync;
        std::function<bool(FunctionDef*)> needsCallerContextRefresh;
        std::function<void(bool, bool)> ensureContextScopeForCall;
        std::function<FunctionDef*(const ArrayOf&)> functionDefFromHandle;

        auto pushParenSubset = [&](ArrayOf base, ArrayOfVector& indices, uint8_t flags,
                                   uint16_t requestedOutputs) {
            if (requestedOutputs == 1) {
                stack.push_back(logicalScalarFromKeyword(base.asLogicalScalar(), indices));
                return;
            }
            ArrayOf tableSubset;
            if (tryFastTableParenSubset(base, indices, tableSubset)) {
                stack.push_back(tableSubset);
                return;
            }
            ArrayOf mapValue;
            if (requestedOutputs <= 1 && tryFastContainersMapLookup(base, indices, mapValue)) {
                stack.push_back(mapValue);
                return;
            }
            if (base.isFunctionHandle()) {
                if ((flags & INST_FLAG_INDEX_ONLY) && indices.size() == 1 && indices[0].isScalar()
                    && indices[0].asInt64Scalar() == 1) {
                    if ((flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                        Error(L"Nelson:interpreter:invalidFieldAssignment",
                            L"Cannot apply A.field_name = B to non struct-array object A.");
                    }
                    Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                }
                int nout = requestedOutputs > 1 ? static_cast<int>(requestedOutputs)
                                                : ((flags & INST_FLAG_QUIET) ? 0 : 1);
                ArrayOfVector result;
                FunctionDef* handleDef = functionDefFromHandle(base);
                bool syncCallerContext = needsCallerContextSync(handleDef);
                bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                if (syncCallerContext) {
                    syncAssignedLocalsToContext();
                }
                size_t clearEpoch = context->getVariablesClearedEpoch();
                try {
                    result = callFunctionHandle(eval, base, indices, nout);
                } catch (const NelsonException& e) {
                    if (!(flags & INST_FLAG_PRINT) || nout != 1
                        || (e.identifier() != L"Nelson:tooManyOutputs"
                            && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                            && e.message()
                                != Nelson::resolveMessageTemplate(
                                    L"Nelson:interpreter:notEnoughVarargout",
                                    L"Not enough outputs in varargout to satisfy call."))) {
                        throw;
                    }
                    nout = 0;
                    result = callFunctionHandle(eval, base, indices, nout);
                }
                refreshAfterVariableClear(clearEpoch);
                if (refreshCallerContext) {
                    refreshLocalsFromContext();
                }
                if (nout == 0) {
                    stack.push_back(noOutputSentinel());
                    return;
                }
                for (int k = 0; k < nout; ++k) {
                    stack.push_back(
                        k < static_cast<int>(result.size()) ? result[k] : ArrayOf::empty());
                }
                return;
            }
            if (base.isHandle()) {
                stringVector subtypes;
                ArrayOfVector subsindices;
                subtypes.push_back("()");
                subsindices.push_back(classIndexCell(indices));
                bool haveFunction = false;
                int nout = requestedOutputs > 1 ? static_cast<int>(requestedOutputs) : 1;
                ArrayOfVector result
                    = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction, nout);
                if (haveFunction) {
                    for (int k = 0; k < nout; ++k) {
                        stack.push_back(
                            k < static_cast<int>(result.size()) ? result[k] : ArrayOf::empty());
                    }
                    return;
                }
            }
            if (base.isClassType()) {
                stringVector subtypes;
                ArrayOfVector subsindices;
                subtypes.push_back("()");
                subsindices.push_back(classIndexCell(indices));
                bool haveFunction = false;
                int nout = requestedOutputs > 1 ? static_cast<int>(requestedOutputs) : 1;
                ArrayOfVector result
                    = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction, nout);
                if (!haveFunction) {
                    // No paren-read hook fired: a classdef object with no subsref, or a classic
                    // (struct-based) object indexed inside its own method. Both index the object
                    // array like a struct (vectorSubset handles logical and numeric indices), so
                    // obj(mask) works in a method.
                    if (classdefLoadClass(base.classType())
                        || base.dataClass() == NLS_CLASS_ARRAY) {
                        if (requestedOutputs > 1) {
                            Error(L"Nelson:interpreter:insufficientOutputs",
                                L"Insufficient number of outputs from right hand side of equal "
                                L"sign to satisfy assignment.");
                        }
                        if (indices.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        if (indices.size() == 1) {
                            stack.push_back(base.vectorSubset(indices[0]));
                        } else {
                            stack.push_back(base.nDimSubset(indices));
                        }
                        return;
                    }
                    Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                }
                for (int k = 0; k < nout; ++k) {
                    stack.push_back(
                        k < static_cast<int>(result.size()) ? result[k] : ArrayOf::empty());
                }
                return;
            }
            resolveObjectIndices(eval, indices);
            try {
                if (requestedOutputs > 1) {
                    Error(L"Nelson:interpreter:insufficientOutputs",
                        L"Insufficient number of outputs from right hand side of equal sign to "
                        L"satisfy assignment.");
                }
                if (indices.empty()) {
                    Error(L"Nelson:interpreter:indexExpressionExpected",
                        L"Index expression expected.");
                }
                ArrayOf directMaskSubset;
                if (indices.size() == 1
                    && tryDoubleLogicalMaskSubsetDirect(base, indices[0], directMaskSubset)) {
                    stack.push_back(directMaskSubset);
                    return;
                }
                ArrayOf directLinearSubset;
                if (indices.size() == 1
                    && tryDoubleLinearSubsetDirect(base, indices[0], directLinearSubset)) {
                    stack.push_back(directLinearSubset);
                    return;
                }
                ArrayOf directSubset;
                if (indices.size() == 2
                    && tryDoubleMatrix2DSubsetDirect(base, indices[0], indices[1], directSubset)) {
                    stack.push_back(directSubset);
                    return;
                }
                if (indices.size() == 1) {
                    stack.push_back(base.vectorSubset(indices[0]));
                } else {
                    stack.push_back(base.nDimSubset(indices));
                }
            } catch (const NelsonException& e) {
                if ((flags & INST_FLAG_ALLOW_UNDEFINED) != 0
                    && e.identifier() == L"Nelson:badsubscript") {
                    checkDotAssignParent(base);
                }
                if ((flags & INST_FLAG_ALLOW_UNDEFINED) != 0
                    && (e.identifier() == L"Nelson:badsubscript"
                        || e.message()
                            == Nelson::resolveMessageTemplate(
                                L"Nelson:interpreter:invalidDimensions", L"Invalid dimensions."))
                    && (base.isStruct() || base.isEmpty())) {
                    if (base.isStruct()) {
                        if (base.isEmpty()) {
                            stack.push_back(base);
                        } else {
                            ArrayOf first = ArrayOf::integerRange(1, 1, 1, false);
                            stack.push_back(base.vectorSubset(first));
                        }
                    } else {
                        stack.push_back(ArrayOf::empty());
                    }
                    return;
                }
                throw;
            }
        };

        auto tryPushDirectScalarVectorSubsetAt = [&](ArrayOf& base, double scalarIndex) -> bool {
            if (Profiler::getInstance()->isOn() || base.isSparse()
                || (!base.isNumeric() && !base.isLogical())) {
                return false;
            }
            const auto ordinal = static_cast<uint64>(scalarIndex);
            if (scalarIndex < 1.0 || scalarIndex != static_cast<double>(ordinal)) {
                return false;
            }
            stack.push_back(base.valueAtIndex(ordinal - 1));
            return true;
        };

        auto tryPushDirectScalarVectorSubset = [&](ArrayOf& base, const ArrayOf& index) -> bool {
            if (!index.isDoubleType(true) || index.elementCount() != 1) {
                return false;
            }
            return tryPushDirectScalarVectorSubsetAt(
                base, *static_cast<const double*>(index.dataPointer()));
        };

        auto tryGetDirectDoubleVectorValue
            = [&](const ArrayOf& base, const ArrayOf& index, double& value) -> bool {
            if (base.isSparse() || !base.isDoubleType(true) || !index.isDoubleType(true)
                || index.elementCount() != 1) {
                return false;
            }
            const double scalarIndex = *static_cast<const double*>(index.dataPointer());
            const auto ordinal = static_cast<uint64>(scalarIndex);
            if (scalarIndex < 1.0 || scalarIndex != static_cast<double>(ordinal)
                || ordinal > static_cast<uint64>(base.elementCount())) {
                return false;
            }
            const auto* data = static_cast<const double*>(base.dataPointer());
            value = data[ordinal - 1];
            return true;
        };

        auto executeLoadParensLocal = [&](const Instruction& loadIns) {
            if (loadIns.A >= frame.locals.size() || loadIns.B >= frame.locals.size()) {
                Error(L"Nelson:bytecode:localSlotOutOfRange",
                    L"BytecodeVM: local slot out of range.");
            }
            ArrayOf* base = nullptr;
            ArrayOf contextBase;
            if (isRuntimeAssignedLocalSlot(frame, loadIns.A)) {
                base = &frame.locals[loadIns.A];
            } else if (loadIns.A < chunk.localNames.size()
                && !chunk.localNames[loadIns.A].empty()) {
                if (!context->lookupVariable(chunk.localNames[loadIns.A], contextBase)) {
                    raiseUndefinedVariable(context, chunk, chunk.localNames[loadIns.A]);
                }
                base = &contextBase;
            } else {
                Error(L"Nelson:UndefinedVariable:undefinedVariable", L"Undefined variable.");
            }
            const ArrayOf* index = nullptr;
            if (isRuntimeAssignedLocalSlot(frame, loadIns.B)) {
                index = &frame.locals[loadIns.B];
            } else if (loadIns.B < chunk.localNames.size()
                && !chunk.localNames[loadIns.B].empty()) {
                ArrayOf contextIndex;
                if (!context->lookupVariable(chunk.localNames[loadIns.B], contextIndex)) {
                    raiseUndefinedVariable(context, chunk, chunk.localNames[loadIns.B]);
                }
                frame.locals[loadIns.B] = contextIndex;
                markRuntimeAssigned(frame, loadIns.B);
                index = &frame.locals[loadIns.B];
            } else {
                Error(L"Nelson:UndefinedVariable:undefinedVariable", L"Undefined variable.");
            }

            ArrayOf offsetIndex;
            const ArrayOf* effectiveIndex = index;
            if (loadIns.op == OpCode::LOAD_PARENS_LOCAL_OFFSET) {
                ArrayOf offset = constantAsArray(chunk.constants.get(loadIns.C));
                if (!frame.chainCollect.active && index->isDoubleType(true)
                    && index->elementCount() == 1 && offset.isDoubleType(true)
                    && offset.elementCount() == 1) {
                    const double scalarIndex = *static_cast<const double*>(index->dataPointer())
                        + *static_cast<const double*>(offset.dataPointer());
                    if (tryPushDirectScalarVectorSubsetAt(*base, scalarIndex)) {
                        return;
                    }
                }
                offsetIndex = binaryOperator(eval, OpCode::OP_PLUS, *index, offset);
                effectiveIndex = &offsetIndex;
            } else if (loadIns.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL) {
                if (loadIns.C >= frame.locals.size()) {
                    Error(L"Nelson:bytecode:localSlotOutOfRange",
                        L"BytecodeVM: local slot out of range.");
                }
                const ArrayOf* offset = nullptr;
                if (isRuntimeAssignedLocalSlot(frame, loadIns.C)) {
                    offset = &frame.locals[loadIns.C];
                } else if (loadIns.C < chunk.localNames.size()
                    && !chunk.localNames[loadIns.C].empty()) {
                    ArrayOf contextOffset;
                    if (!context->lookupVariable(chunk.localNames[loadIns.C], contextOffset)) {
                        raiseUndefinedVariable(context, chunk, chunk.localNames[loadIns.C]);
                    }
                    frame.locals[loadIns.C] = contextOffset;
                    markRuntimeAssigned(frame, loadIns.C);
                    offset = &frame.locals[loadIns.C];
                } else {
                    Error(L"Nelson:UndefinedVariable:undefinedVariable", L"Undefined variable.");
                }
                if (!frame.chainCollect.active && index->isDoubleType(true)
                    && index->elementCount() == 1 && offset->isDoubleType(true)
                    && offset->elementCount() == 1) {
                    const double scalarIndex = *static_cast<const double*>(index->dataPointer())
                        - *static_cast<const double*>(offset->dataPointer());
                    if (tryPushDirectScalarVectorSubsetAt(*base, scalarIndex)) {
                        return;
                    }
                }
                offsetIndex = binaryOperator(eval, OpCode::OP_MINUS, *index, *offset);
                effectiveIndex = &offsetIndex;
            }

            if (frame.chainCollect.active) {
                ArrayOfVector collectIdx;
                collectIdx.push_back(*effectiveIndex);
                frame.chainCollect.subtypes.push_back("()");
                frame.chainCollect.subsindices.push_back(classIndexCell(collectIdx));
                stack.push_back(ArrayOf::empty());
                return;
            }
            if ((loadIns.op == OpCode::LOAD_PARENS_LOCAL_OFFSET
                    || loadIns.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL || loadIns.C <= 1)
                && tryPushDirectScalarVectorSubset(*base, *effectiveIndex)) {
                return;
            }
            ArrayOfVector indices;
            indices.push_back(*effectiveIndex);
            ArrayOf baseValue = *base;
            pushParenSubset(baseValue, indices, loadIns.flags,
                (loadIns.op == OpCode::LOAD_PARENS_LOCAL_OFFSET
                    || loadIns.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL)
                    ? 0
                    : loadIns.C);
        };

        auto invokeFunctionHandleValue = [&](const ArrayOf& value, const ArrayOfVector& params,
                                             int requestedOutputs) -> ArrayOfVector {
            FunctionDef* handleDef = functionDefFromHandle(value);
            bool syncCallerContext = needsCallerContextSync(handleDef);
            bool refreshCallerContext = needsCallerContextRefresh(handleDef);
            ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
            if (syncCallerContext) {
                syncAssignedLocalsToContext();
            }
            size_t clearEpoch = context->getVariablesClearedEpoch();
            ArrayOfVector result = callFunctionHandle(eval, value, params, requestedOutputs);
            refreshAfterVariableClear(clearEpoch);
            if (refreshCallerContext) {
                refreshLocalsFromContext();
            }
            return result;
        };

        auto indexFirstValue
            = [&](const ArrayOfVector& values, const ArrayOfVector& params) -> ArrayOfVector {
            ArrayOf value = values.empty() ? ArrayOf::empty() : values[0];
            if (value.isFunctionHandle()) {
                return invokeFunctionHandleValue(value, params, 1);
            }
            if (value.isClassType() || value.isHandle()) {
                stringVector subtypes;
                ArrayOfVector subsindices;
                subtypes.push_back("()");
                subsindices.push_back(classIndexCell(params));
                bool haveFunction = false;
                ArrayOfVector result
                    = eval->bytecodeExtractClass(value, subtypes, subsindices, haveFunction, 1);
                if (haveFunction) {
                    return result;
                }
            }
            ArrayOfVector mutableParams = params;
            resolveObjectIndices(eval, mutableParams);
            ArrayOfVector result;
            if (mutableParams.size() == 1) {
                result.push_back(value.vectorSubset(mutableParams[0]));
            } else {
                result.push_back(value.nDimSubset(mutableParams));
            }
            return result;
        };

        auto evaluateDotReference
            = [&](const ArrayOf& base, const std::string& field, const ArrayOfVector& params,
                  bool explicitCall = false) -> ArrayOfVector {
            // `explicitCall` records the parenthesised form `obj.member(...)` even with an empty
            // argument list. Without it `obj.method()` looks like a property read `obj.method` and
            // raises "No such property" instead of invoking the method.
            const bool callForm = !params.empty() || explicitCall;
            if (base.isClassType()) {
                if (callForm) {
                    ArrayOf propertyValue;
                    if (tryReadClassdefPlainProperty(eval, base, field, propertyValue)
                        && propertyValue.isFunctionHandle()) {
                        return invokeFunctionHandleValue(propertyValue, params, 1);
                    }
                }
                if (base.dataClass() == NLS_CLASS_ARRAY
                    && eval->getClassdefAccessContext() == base.classType()) {
                    // Inside its own method, a classic (struct-based) object reads its data field
                    // directly. obj.field(args) then indexes the field's value instead of going
                    // through subsref.
                    bool fieldExists = false;
                    for (const std::string& existing : base.fieldNames()) {
                        if (existing == field) {
                            fieldExists = true;
                            break;
                        }
                    }
                    if (fieldExists) {
                        ArrayOfVector fv = base.fieldAsList(field);
                        ArrayOf fieldValue = fv.empty() ? ArrayOf::empty() : fv[0];
                        if (params.empty()) {
                            return { fieldValue };
                        }
                        return { subsetClassicFieldValue(eval, fieldValue, params) };
                    }
                }
                stringVector subtypes;
                ArrayOfVector subsindices;
                subtypes.push_back(".");
                subsindices.push_back(ArrayOf::characterArray(field));
                if (callForm) {
                    subtypes.push_back("()");
                    subsindices.push_back(classIndexCell(params));
                }
                bool haveFunction = false;
                ArrayOfVector result
                    = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                if (!haveFunction) {
                    if (callForm) {
                        ArrayOfVector methodResult;
                        // A property accessed with arguments (obj.prop(args)) is a field access
                        // followed by an index/call, not a method call, even if a method differing
                        // only in case exists. The match is case-sensitive: obj.name(args) still
                        // calls method 'name' when only property 'Name' exists.
                        ClassdefPropertyDefinition propertyForCall;
                        const bool isProperty = base.isScalar() && base.isClassType()
                            && classdefLoadClass(base.classType())
                            && classdefPropertyDefinition(base.classType(), field, propertyForCall)
                            && propertyForCall.name == field;
                        if (!isProperty
                            && eval->bytecodeInvokeObjectMethodIfExists(
                                base, field, params, 1, methodResult)) {
                            return methodResult;
                        }
                    }
                    ArrayOfVector fieldValues = getClassdefFieldAsList(eval, base, field);
                    return params.empty() ? fieldValues : indexFirstValue(fieldValues, params);
                }
                return result;
            }
            if (base.isHandle() || base.isGraphicsObject()) {
                return getOrInvokeHandle(eval, base, field, params);
            }
            if (!base.isStruct()) {
                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
            }
            ArrayOfVector fieldValues = base.fieldAsList(field);
            if (!params.empty()) {
                return indexFirstValue(fieldValues, params);
            }
            return fieldValues;
        };

        // Walk the middle segments of a CALL_CLASSDEF_MEMBER class spec
        // ("head.f1.f2" -> fields f1, f2) as member reads on `head`.
        auto applyClassdefMemberMiddlePath
            = [&](const ArrayOf& head, const std::string& className) -> ArrayOf {
            const size_t headDot = className.find('.');
            ArrayOf current = head;
            if (headDot == std::string::npos) {
                return current;
            }
            const std::string middlePath = className.substr(headDot + 1);
            size_t start = 0;
            while (start <= middlePath.size()) {
                const size_t next = middlePath.find('.', start);
                const std::string field = next == std::string::npos
                    ? middlePath.substr(start)
                    : middlePath.substr(start, next - start);
                ArrayOfVector fieldResult = evaluateDotReference(current, field, {});
                current = fieldResult.empty() ? ArrayOf::empty() : fieldResult[0];
                if (next == std::string::npos) {
                    break;
                }
                start = next + 1;
            }
            return current;
        };

        auto setAssignedLocalByName = [&](const std::string& name, const ArrayOf& value) -> bool {
            size_t count = std::min(chunk.localNames.size(), frame.locals.size());
            bool found = false;
            for (size_t k = 0; k < count; ++k) {
                if (chunk.localNames[k] == name) {
                    frame.locals[k] = value;
                    markRuntimeAssigned(frame, k);
                    found = true;
                }
            }
            return found;
        };

        needsCallerContextSync = [&](FunctionDef* funcDef) -> bool {
            return BytecodeVM::functionNeedsCallerContext(eval, funcDef, false);
        };

        needsCallerContextRefresh = [&](FunctionDef* funcDef) -> bool {
            return BytecodeVM::functionNeedsCallerContext(eval, funcDef, true);
        };

        // Scope-free frames get a context scope on demand the first time a
        // runtime-resolved callee turns out to need the caller context (e.g. an
        // overload picked at dispatch time that the static analysis in
        // bytecodeNeedsContextScope could not see). executeFunction pops it on
        // every exit path.
        ensureContextScopeForCall = [&](bool syncRequired, bool refreshRequired) {
            if ((syncRequired || refreshRequired) && !frame.hasContextScope) {
                context->pushScope(chunk.functionName, chunk.sourcePath);
                frame.hasContextScope = true;
                frame.lazyScopePushed = true;
            }
        };

        auto tryEvaluateDirectSelfBytecode = [&](FunctionDef* funcDef, const ArrayOfVector& args,
                                                 uint16_t nout, ArrayOfVector& result) -> bool {
            BytecodeChunk* calleeChunk = nullptr;
            const ArrayOf* argsData = args.empty() ? nullptr : &args[0];
            if (!canDirectSelfBytecodeCall(
                    eval, chunk, funcDef, argsData, args.size(), nout, calleeChunk)
                || calleeChunk == nullptr) {
                return false;
            }
            auto* macroDef = dynamic_cast<MacroFunctionDef*>(funcDef);
            if (macroDef->inputArgCount() != -1 && args.size() > macroDef->arguments.size()) {
                Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
            }
            // A direct call pushes no scope for the callee: the callee frame must
            // not claim one, or its exit would empty the caller's scope.
            if (nout == 1 && args.size() == 1 && calleeChunk->argNames.size() == 1
                && calleeChunk->argNames[0] != "varargin" && calleeChunk->retNames.size() == 1) {
                result.resize(1);
                result[0] = executeFunctionOneArgOneOutput(eval, *calleeChunk, args[0], false);
            } else {
                result = BytecodeVM::executeFunction(
                    eval, *calleeChunk, args, static_cast<int>(nout), false);
            }
            State state(eval->getState());
            if (state < NLS_STATE_QUIT) {
                eval->resetState();
            }
            if (state == NLS_STATE_ABORT || state == NLS_STATE_QUIT
                || state == NLS_STATE_FORCE_QUIT) {
                result = scalarArrayOfToArrayOfVector(ArrayOf::empty());
            }
            for (size_t k = 0; k < result.size(); ++k) {
                result[k].name("");
            }
            return true;
        };

        functionDefFromHandle = [](const ArrayOf& handle) -> FunctionDef* {
            if (!handle.isFunctionHandle()) {
                return nullptr;
            }
            function_handle fh = handle.asFunctionHandle();
            return reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
        };

        uint32_t loopBackedgeCounter = 0;
        ScalarRangeLoopCache scalarRanges;
        auto pollLoopBackedge = [&]() {
            if ((++loopBackedgeCounter & 31U) == 0) {
                pollInterrupt(eval);
            }
        };
        auto applyImmediateBranch = [&](const Instruction& branch, bool condition) -> bool {
            bool jump = false;
            switch (branch.op) {
            case OpCode::WHILE_CHECK:
            case OpCode::JUMP_IF_FALSE:
            case OpCode::JUMP_IF_FALSE_SCALAR:
                jump = !condition;
                break;
            case OpCode::JUMP_IF_TRUE:
            case OpCode::JUMP_IF_TRUE_SCALAR:
                jump = condition;
                break;
            default:
                return false;
            }
            ++pc;
            if (jump) {
                if (branch.A < pc) {
                    pollLoopBackedge();
                }
                pc = branch.A;
            }
            return true;
        };
        auto tryEvaluateNamedScalarBuiltinFromStack
            = [&](const Instruction& call, const std::string& name) -> bool {
            ScalarBuiltinFastPath fastPath = scalarBuiltinFastPathForName(name, call.B);
            if ((call.flags & INST_FLAG_PRINT) != 0 || call.C != 1
                || !isScalarBuiltinFastPathCandidate(fastPath) || stack.size() < call.B) {
                return false;
            }
            for (uint16_t k = 0; k < call.B; ++k) {
                if (isCommaListSentinel(stack[stack.size() - 1 - k])) {
                    return false;
                }
            }
            ArrayOf variable;
            bool hasVariable = tryGetAssignedLocalByNameIndex(call.A, variable);
            if (!hasVariable) {
                hasVariable = context->lookupVariable(name, variable);
            }
            if (hasVariable) {
                return false;
            }
            FunctionDef* fdef = nullptr;
            if (!lookupCachedScalarBuiltin(call.A, name, fdef)) {
                return false;
            }
            if (call.B == 2) {
                const ArrayOf& lhs = stack[stack.size() - 2];
                const ArrayOf& rhs = stack[stack.size() - 1];
                double scalarBinaryValue = 0.;
                if (tryEvaluateScalarBuiltinFastPathArgs(
                        fastPath, fdef, lhs, rhs, scalarBinaryValue)) {
                    if (pc + 2 < chunk.code.size()) {
                        const Instruction& zeroLoad = chunk.code[pc];
                        const Instruction& comparison = chunk.code[pc + 1];
                        const Instruction& branch = chunk.code[pc + 2];
                        bool comparisonValue = false;
                        if (zeroLoad.op == OpCode::LOAD_CONST
                            && isCachedScalarDoubleZeroConstant(zeroLoad.A)
                            && tryCompareScalarDoubleWithZero(
                                comparison.op, scalarBinaryValue, comparisonValue)
                            && (branch.op == OpCode::WHILE_CHECK
                                || branch.op == OpCode::JUMP_IF_FALSE
                                || branch.op == OpCode::JUMP_IF_FALSE_SCALAR
                                || branch.op == OpCode::JUMP_IF_TRUE
                                || branch.op == OpCode::JUMP_IF_TRUE_SCALAR)) {
                            stack.pop_back();
                            stack.pop_back();
                            pc += 2;
                            applyImmediateBranch(branch, comparisonValue);
                            return true;
                        }
                    }
                    stack.pop_back();
                    stack.pop_back();
                    stack.push_back(ArrayOf::doubleScalar(scalarBinaryValue));
                    return true;
                }
                ArrayOf inlineResult;
                if (tryEvaluateScalarBuiltinFastPathArgs(fastPath, fdef, lhs, rhs, inlineResult)) {
                    stack.pop_back();
                    stack.pop_back();
                    stack.push_back(inlineResult);
                    return true;
                }
                return false;
            }
            ArrayOf inlineResult;
            if (!tryEvaluateScalarBuiltinFastPathArg(fastPath, fdef, stack.back(), inlineResult)) {
                return false;
            }
            stack.pop_back();
            stack.push_back(inlineResult);
            return true;
        };
        bool hasStatementContext = false;
        size_t currentStatementContextId = 0;
        auto clearStatementContext = [&]() {
            if (hasStatementContext) {
                eval->callstack.popID();
                hasStatementContext = false;
                currentStatementContextId = 0;
            }
        };
        auto setStatementContext = [&](const Instruction& statementInstruction) {
            if (statementInstruction.A >= chunk.spans.size()) {
                clearStatementContext();
                return;
            }
            const SourceSpan& span = chunk.spans[statementInstruction.A];
            size_t contextId
                = static_cast<size_t>((static_cast<uint32_t>(span.col) << 16) | span.line);
            if (hasStatementContext && currentStatementContextId == contextId) {
                return;
            }
            clearStatementContext();
            eval->callstack.pushID(contextId);
            hasStatementContext = true;
            currentStatementContextId = contextId;
        };
        auto scalarRangeBoundary = [&](const Instruction& boundary) {
            if (frame.skipDebugStatements) {
                return !PendingHandleFinalizers::hasPending();
            }
            setStatementContext(boundary);
            return quietScalarRangeBoundary(eval, statementBoundaryCounter);
        };
        while (pc < chunk.code.size()) {
            const Instruction& ins = chunk.code[pc++];
            try {
                if (PendingHandleFinalizers::hasPending()) {
                    // Handle objects whose last reference went away during the
                    // previous instruction (clear, reassignment, a consumed
                    // temporary) run their destructor before anything else.
                    DrainPendingHandleFinalizers(eval);
                }
                switch (ins.op) {
                case OpCode::NOP:
                    break;
                case OpCode::DEBUG_STMT:
                    if (frame.skipDebugStatements) {
                        break;
                    }
                    setStatementContext(ins);
                    processStatementBoundaryQueues();
                    if (eval->hasEnabledBreakpoints()
                        && handleDebugStatement(eval, frame, ins, syncAssignedLocalsToContext,
                            refreshLocalsFromContextClearingMissing)) {
                        clearStatementContext();
                        return;
                    }
                    break;
                case OpCode::KEYBOARD:
                    if (handleKeyboardStatement(eval, frame, ins, syncAssignedLocalsToContext,
                            refreshLocalsFromContextClearingMissing)) {
                        clearStatementContext();
                        return;
                    }
                    break;
                case OpCode::CHECK_INTERRUPT: {
                    pollInterrupt(eval);
                } break;
                case OpCode::LOAD_CONST:
                    stack.push_back(constantAsArray(chunk.constants.get(ins.A)));
                    break;
                case OpCode::LOAD_TRUE:
                    stack.push_back(ArrayOf::logicalScalar(true));
                    break;
                case OpCode::LOAD_FALSE:
                    stack.push_back(ArrayOf::logicalScalar(false));
                    break;
                case OpCode::LOAD_EMPTY:
                    stack.push_back(ArrayOf::empty());
                    break;
                case OpCode::LOAD_EMPTY_CELL: {
                    ArrayOf value(ArrayOf::empty());
                    value.promoteType(NLS_CELL_ARRAY);
                    stack.push_back(value);
                } break;
                case OpCode::MAKE_FH_NAMED: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    stack.push_back(makeNamedHandle(eval, frame, chunk.names[ins.A]));
                } break;
                case OpCode::MAKE_FH_ANONYMOUS: {
                    const auto& content = std::get<std::string>(chunk.constants.get(ins.A));
                    const auto& argsPayload = std::get<std::string>(chunk.constants.get(ins.B));
                    const auto& definition = std::get<std::string>(chunk.constants.get(ins.C));
                    stack.push_back(makeAnonymousHandle(
                        eval, frame, content, splitAnonymousArguments(argsPayload), definition));
                } break;
                case OpCode::DECLARE_GLOBAL:
                case OpCode::DECLARE_PERSISTENT: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    if (ins.op == OpCode::DECLARE_GLOBAL) {
                        declareGlobalVariable(context, chunk.names[ins.A]);
                    } else {
                        declarePersistentVariable(context, chunk.names[ins.A], chunk.isScript);
                    }
                } break;
                case OpCode::DECLARE_IMPORT:
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    context->addImport(chunk.names[ins.A]);
                    break;
                case OpCode::LOAD_LOCAL:
                    executeLoadLocal(ins);
                    break;
                case OpCode::SCALAR_EXPR: {
                    const ScalarExprProgram& program = chunk.scalarExprPrograms[ins.A];
                    ScalarExprOutcome outcome = ScalarExprOutcome::Refused;
                    if (!Profiler::getInstance()->isOn()) {
                        outcome
                            = tryExecuteScalarExprProgram(frame, chunk, context, stack, program);
                    }
                    if (outcome != ScalarExprOutcome::Refused) {
                        // pc already points past SCALAR_EXPR, so the first
                        // instruction of the window is at pc - 1.
                        const uint32_t afterWindow = (pc - 1) + program.windowLength;
                        if (outcome == ScalarExprOutcome::BranchTaken) {
                            if (program.branchTarget < afterWindow) {
                                pollLoopBackedge();
                            }
                            pc = program.branchTarget;
                        } else {
                            pc = afterWindow;
                        }
                        break;
                    }
                    // typed fast path refused: run the original first
                    // instruction, the rest of the window is still in place.
                    if (program.original.op == OpCode::LOAD_CONST) {
                        stack.push_back(constantAsArray(chunk.constants.get(program.original.A)));
                    } else if (program.original.op == OpCode::LOAD_PARENS_LOCAL
                        || program.original.op == OpCode::LOAD_PARENS_LOCAL_OFFSET
                        || program.original.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL) {
                        executeLoadParensLocal(program.original);
                    } else {
                        executeLoadLocal(program.original);
                    }
                } break;
                case OpCode::LOAD_LOCAL_DOT: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.B >= chunk.constants.size()) {
                        Error(L"Nelson:bytecode:fieldConstantIndexOutOfRange",
                            L"BytecodeVM: field constant index out of range.");
                    }
                    ArrayOf base;
                    if (isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        base = frame.locals[ins.A];
                    } else if (ins.A < chunk.localNames.size()
                        && !chunk.localNames[ins.A].empty()) {
                        if (!context->lookupVariable(chunk.localNames[ins.A], base)) {
                            raiseUndefinedVariable(context, chunk, chunk.localNames[ins.A]);
                        }
                    } else {
                        Error(
                            L"Nelson:UndefinedVariable:undefinedVariable", L"Undefined variable.");
                    }
                    const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                    ArrayOf directValue;
                    if (tryReadStructScalarFieldDirect(base, field, directValue)
                        || tryFastTableDotVariable(eval, base, field, directValue)) {
                        stack.push_back(directValue);
                        break;
                    }
                    if (base.isClassType()) {
                        if (tryReadClassdefPlainProperty(eval, base, field, directValue)) {
                            stack.push_back(directValue);
                            break;
                        }
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArray(field));
                        bool haveFunction = false;
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            base, subtypes, subsindices, haveFunction, 1);
                        if (!haveFunction) {
                            // a no-argument method accessed without parentheses on an
                            // instance held in a variable (obj.method) is invoked, not
                            // read as a property.
                            ArrayOfVector methodResult;
                            if (base.isScalar() && classdefLoadClass(base.classType())
                                && !classdefHasProperty(base.classType(), field)
                                && eval->bytecodeInvokeObjectMethodIfExists(
                                    base, field, {}, 1, methodResult)) {
                                stack.push_back(
                                    methodResult.empty() ? ArrayOf::empty() : methodResult[0]);
                                break;
                            }
                            result = getClassdefFieldAsList(eval, base, field);
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (base.isHandle() || base.isGraphicsObject()) {
                        ArrayOfVector params;
                        ArrayOfVector result = getOrInvokeHandle(eval, base, field, params);
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (!base.isStruct()) {
                        Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                    }
                    ArrayOfVector fieldValues = base.fieldAsList(field);
                    stack.push_back(fieldValues.empty() ? ArrayOf::empty() : fieldValues[0]);
                } break;
                case OpCode::LOAD_PARENS_LOCAL:
                case OpCode::LOAD_PARENS_LOCAL_OFFSET:
                case OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL:
                    executeLoadParensLocal(ins);
                    break;
                case OpCode::LOAD_BRACES_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    const ArrayOf* base = nullptr;
                    ArrayOf contextBase;
                    if (isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        base = &frame.locals[ins.A];
                    } else if (ins.A < chunk.localNames.size()
                        && !chunk.localNames[ins.A].empty()) {
                        if (!context->lookupVariable(chunk.localNames[ins.A], contextBase)) {
                            raiseUndefinedVariable(context, chunk, chunk.localNames[ins.A]);
                        }
                        base = &contextBase;
                    } else {
                        Error(
                            L"Nelson:UndefinedVariable:undefinedVariable", L"Undefined variable.");
                    }
                    const ArrayOf* index = nullptr;
                    if (isRuntimeAssignedLocalSlot(frame, ins.B)) {
                        index = &frame.locals[ins.B];
                    } else if (ins.B < chunk.localNames.size()
                        && !chunk.localNames[ins.B].empty()) {
                        ArrayOf contextIndex;
                        if (!context->lookupVariable(chunk.localNames[ins.B], contextIndex)) {
                            raiseUndefinedVariable(context, chunk, chunk.localNames[ins.B]);
                        }
                        frame.locals[ins.B] = contextIndex;
                        markRuntimeAssigned(frame, ins.B);
                        index = &frame.locals[ins.B];
                    } else {
                        Error(
                            L"Nelson:UndefinedVariable:undefinedVariable", L"Undefined variable.");
                    }
                    ArrayOf directValue;
                    if (tryReadCellScalarContentDirect(*base, *index, directValue)) {
                        stack.push_back(directValue);
                        break;
                    }
                    ArrayOfVector indices;
                    indices.push_back(*index);
                    ArrayOf baseValue = *base;
                    if (baseValue.isClassType() || baseValue.isHandle()) {
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back("{}");
                        subsindices.push_back(classIndexCell(indices));
                        bool haveFunction = false;
                        int nout = 1;
                        if ((ins.flags & INST_FLAG_LIST) != 0) {
                            // comma-separated-list context: the class decides how
                            // many values via numArgumentsFromSubscript
                            int reported = objectNumArgumentsFromSubscript(
                                eval, baseValue, subtypes, subsindices, "Expression");
                            if (reported >= 0) {
                                nout = reported;
                            }
                        }
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            baseValue, subtypes, subsindices, haveFunction, nout);
                        if (!haveFunction && baseValue.isClassType()) {
                            Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                        }
                        if (haveFunction) {
                            if ((ins.flags & INST_FLAG_LIST) != 0 && result.size() != 1) {
                                stack.push_back(commaListSentinel(result));
                            } else {
                                stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                            }
                            break;
                        }
                    }
                    resolveObjectIndices(eval, indices);
                    ArrayOfVector values = baseValue.vectorContentsAsList(indices[0]);
                    if ((ins.flags & INST_FLAG_LIST) != 0 && values.size() != 1) {
                        // comma-list context: zero or multiple extracted values
                        // must expand at the consuming opcode
                        stack.push_back(commaListSentinel(values));
                        break;
                    }
                    if (values.empty()) {
                        Error(L"Nelson:interpreter:emptyExpression", L"Empty expression!");
                    }
                    stack.push_back(values[0]);
                } break;
                case OpCode::LOAD_LOCAL_COUNT_RANGE: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    ArrayOf value;
                    if (isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        value = frame.locals[ins.A];
                    } else if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->lookupVariable(chunk.localNames[ins.A], value)) {
                    } else {
                        value = ArrayOf::empty();
                    }
                    stack.push_back(ArrayOf::integerRange(1, 1, value.elementCount(), false));
                } break;
                case OpCode::LOAD_CONTENT_TARGET_COUNT: {
                    ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                    ArrayOf base = pop();
                    if (!base.isClassType() && !base.isHandle()) {
                        resolveObjectIndices(eval, indices);
                    }
                    expandColonContentAssignmentIndices(base, indices);
                    indexType count = contentAssignmentTargetCount(indices);
                    stack.push_back(ArrayOf::integerRange(1, 1, count, false));
                } break;
                case OpCode::STORE_LOCAL:
                case OpCode::ASSIGN:
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->isLockedVariable(chunk.localNames[ins.A])) {
                        Error(L"Nelson:interpreter:redefinePermanent",
                            L"Redefining permanent variable.");
                    }
                    if (valueOwnsObject(frame.locals[ins.A])) {
                        // previous value is a handle or a function handle (which
                        // owns its captured workspace): its context copy (if
                        // any) must not outlive this store
                        ArrayOf stored = pop();
                        refreshStaleContextHandleCopy(ins.A, stored);
                        frame.locals[ins.A] = std::move(stored);
                    } else if (ins.op == OpCode::ASSIGN) {
                        profiledVoidOperation(
                            eval, SUBSASGN_OPERATOR_STR, [&]() { frame.locals[ins.A] = pop(); });
                    } else {
                        frame.locals[ins.A] = pop();
                    }
                    markRuntimeAssigned(frame, ins.A);
                    break;
                case OpCode::ASSIGN_PLUS_CELL_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->isLockedVariable(chunk.localNames[ins.A])) {
                        Error(L"Nelson:interpreter:redefinePermanent",
                            L"Redefining permanent variable.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf& target = ensureLocal(ins.A);
                        const ArrayOf& base = ensureLocal(ins.B);
                        const ArrayOf& index = ensureLocal(ins.C);
                        if (!tryAddCellScalarDoubleContentToLocal(target, base, index)) {
                            ArrayOf value;
                            bool haveValue = false;
                            if (!tryReadCellScalarContentDirect(base, index, value)) {
                                ArrayOfVector indices;
                                indices.push_back(index);
                                if (base.isClassType() || base.isHandle()) {
                                    stringVector subtypes;
                                    ArrayOfVector subsindices;
                                    subtypes.push_back("{}");
                                    subsindices.push_back(classIndexCell(indices));
                                    bool haveFunction = false;
                                    ArrayOfVector result = eval->bytecodeExtractClass(
                                        base, subtypes, subsindices, haveFunction, 1);
                                    if (!haveFunction && base.isClassType()) {
                                        Error(L"Nelson:interpreter:invalidIndexing",
                                            L"Invalid indexing.");
                                    }
                                    if (haveFunction) {
                                        value = result.empty() ? ArrayOf::empty() : result[0];
                                        haveValue = true;
                                    }
                                } else {
                                    ArrayOf mutableBase = base;
                                    ArrayOf mutableIndex = index;
                                    value = mutableBase.vectorContents(mutableIndex);
                                    haveValue = true;
                                }
                            } else {
                                haveValue = true;
                            }
                            if (!haveValue) {
                                ArrayOf mutableBase = base;
                                ArrayOf mutableIndex = index;
                                value = mutableBase.vectorContents(mutableIndex);
                            }
                            target = binaryOperator(eval, OpCode::OP_PLUS, target, value);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASSIGN_PLUS_STACK: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    ArrayOf value = pop();
                    ArrayOf targetValue = pop();
                    if (!Profiler::getInstance()->isOn() && targetValue.isDoubleType(true)
                        && value.isDoubleType(true) && targetValue.elementCount() == 1
                        && value.elementCount() == 1) {
                        double result = *static_cast<const double*>(targetValue.dataPointer())
                            + *static_cast<const double*>(value.dataPointer());
                        setLocalDouble(frame.locals, ins.A, result);
                    } else {
                        frame.locals[ins.A]
                            = binaryOperator(eval, OpCode::OP_PLUS, targetValue, value);
                    }
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::ASSIGN_PARENS_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    ArrayOf& target = frame.locals[ins.A];
                    ArrayOf& base = ensureLocal(ins.B);
                    const ArrayOf& index = ensureLocal(ins.C);
                    double indexedValue = 0;
                    if (!Profiler::getInstance()->isOn()
                        && tryGetDirectDoubleVectorValue(base, index, indexedValue)) {
                        setLocalDouble(frame.locals, ins.A, indexedValue);
                        markRuntimeAssigned(frame, ins.A);
                        break;
                    }
                    if (tryPushDirectScalarVectorSubset(base, index)) {
                        target = pop();
                        markRuntimeAssigned(frame, ins.A);
                        break;
                    }
                    ArrayOfVector indices;
                    indices.push_back(index);
                    ArrayOf baseValue = base;
                    pushParenSubset(baseValue, indices, ins.flags, 0);
                    target = pop();
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::ASSIGN_PLUS_PARENS_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    ArrayOf& target = ensureLocal(ins.A);
                    ArrayOf& base = ensureLocal(ins.B);
                    const ArrayOf& index = ensureLocal(ins.C);
                    double indexedValue = 0;
                    if (!Profiler::getInstance()->isOn() && target.isDoubleType(true)
                        && target.elementCount() == 1
                        && tryGetDirectDoubleVectorValue(base, index, indexedValue)) {
                        double result
                            = *static_cast<const double*>(target.dataPointer()) + indexedValue;
                        setLocalDouble(frame.locals, ins.A, result);
                    } else {
                        ArrayOf value;
                        if (tryPushDirectScalarVectorSubset(base, index)) {
                            value = pop();
                        } else {
                            ArrayOfVector indices;
                            indices.push_back(index);
                            ArrayOf baseValue = base;
                            pushParenSubset(baseValue, indices, ins.flags, 0);
                            value = pop();
                        }
                        target = binaryOperator(eval, OpCode::OP_PLUS, target, value);
                    }
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::ASSIGN_PLUS_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    ArrayOf& target = ensureLocal(ins.A);
                    const ArrayOf& value = ensureLocal(ins.B);
                    if (!Profiler::getInstance()->isOn() && target.isDoubleType(true)
                        && value.isDoubleType(true) && target.elementCount() == 1
                        && value.elementCount() == 1) {
                        double result = *static_cast<const double*>(target.dataPointer())
                            + *static_cast<const double*>(value.dataPointer());
                        setLocalDouble(frame.locals, ins.A, result);
                    } else {
                        target = binaryOperator(eval, OpCode::OP_PLUS, target, value);
                    }
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::ASSIGN_PLUS_CONST: {
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.constants.size()) {
                        Error(L"Nelson:bytecode:localSlotOrConstantIndexOutOfRange",
                            L"BytecodeVM: local slot or constant index out of range.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    ArrayOf& target = ensureLocal(ins.A);
                    double increment = 0;
                    const bool haveDoubleIncrement
                        = loadCachedScalarDoubleConstant(ins.B, increment);
                    if (!Profiler::getInstance()->isOn() && haveDoubleIncrement
                        && target.isDoubleType(true) && target.elementCount() == 1) {
                        double result
                            = *static_cast<const double*>(target.dataPointer()) + increment;
                        setLocalDouble(frame.locals, ins.A, result);
                    } else {
                        ArrayOf incrementValue = constantAsArray(chunk.constants.get(ins.B));
                        target = binaryOperator(eval, OpCode::OP_PLUS, target, incrementValue);
                    }
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::ASSIGN_PLUS_RDIV_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= chunk.constants.size()) {
                        Error(L"Nelson:bytecode:localSlotOrConstantIndexOutOfRange",
                            L"BytecodeVM: local slot or constant index out of range.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    ArrayOf& target = ensureLocal(ins.A);
                    const ArrayOf& denominator = ensureLocal(ins.B);
                    double numerator = 0;
                    bool haveDoubleNumerator = loadCachedScalarDoubleConstant(ins.C, numerator);
                    if (!Profiler::getInstance()->isOn() && haveDoubleNumerator
                        && target.isDoubleType(true) && denominator.isDoubleType(true)
                        && target.elementCount() == 1 && denominator.elementCount() == 1) {
                        double result = *static_cast<const double*>(target.dataPointer())
                            + numerator / *static_cast<const double*>(denominator.dataPointer());
                        setLocalDouble(frame.locals, ins.A, result);
                    } else {
                        ArrayOf numeratorValue = constantAsArray(chunk.constants.get(ins.C));
                        ArrayOf quotient
                            = binaryOperator(eval, OpCode::OP_MRDIV, numeratorValue, denominator);
                        target = binaryOperator(eval, OpCode::OP_PLUS, target, quotient);
                    }
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::ASSIGN_PLUS_LOCAL_RDIV_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    auto ensureLocal = [&](uint16_t slot) -> ArrayOf& {
                        if (!isRuntimeAssignedLocalSlot(frame, slot)) {
                            if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()) {
                                if (!context->lookupVariable(
                                        chunk.localNames[slot], frame.locals[slot])) {
                                    raiseUndefinedVariable(context, chunk, chunk.localNames[slot]);
                                }
                                markRuntimeAssigned(frame, slot);
                            } else {
                                Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                    L"Undefined variable.");
                            }
                        }
                        return frame.locals[slot];
                    };
                    ArrayOf& target = ensureLocal(ins.A);
                    const ArrayOf& numerator = ensureLocal(ins.B);
                    const ArrayOf& denominator = ensureLocal(ins.C);
                    if (!Profiler::getInstance()->isOn() && target.isDoubleType(true)
                        && numerator.isDoubleType(true) && denominator.isDoubleType(true)
                        && target.elementCount() == 1 && numerator.elementCount() == 1
                        && denominator.elementCount() == 1) {
                        double result = *static_cast<const double*>(target.dataPointer())
                            + *static_cast<const double*>(numerator.dataPointer())
                                / *static_cast<const double*>(denominator.dataPointer());
                        setLocalDouble(frame.locals, ins.A, result);
                    } else {
                        ArrayOf quotient
                            = binaryOperator(eval, OpCode::OP_MRDIV, numerator, denominator);
                        target = binaryOperator(eval, OpCode::OP_PLUS, target, quotient);
                    }
                    markRuntimeAssigned(frame, ins.A);
                } break;
                case OpCode::LOAD_GLOBAL: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    // B = 1 + local slot of the name: once assigned, the slot
                    // holds the value and the context copy is stale
                    if (ins.B != 0 && isRuntimeAssignedLocalSlot(frame, ins.B - 1u)) {
                        stack.push_back(frame.locals[ins.B - 1u]);
                        break;
                    }
                    const std::string& name = chunk.names[ins.A];
                    ArrayOf value;
                    bool found = false;
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        Scope* globalScope = context->getGlobalScope();
                        found = globalScope != nullptr && globalScope->lookupVariable(name, value);
                    } else {
                        found = context->lookupVariable(name, value);
                    }
                    if (!found) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                        FunctionDef* fdef = nullptr;
                        if (eval->lookupFunction(name, fdef)) {
                            try {
                                ArrayOfVector result = fdef->evaluateFunction(eval, {}, 1);
                                stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                            } catch (const NelsonException& e) {
                                if (e.identifier() != L"Nelson:tooManyOutputs"
                                    && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:interpreter:notEnoughVarargout",
                                            L"Not enough outputs in varargout to satisfy call.")) {
                                    throw;
                                }
                                (void)fdef->evaluateFunction(eval, {}, 0);
                                stack.push_back(noOutputSentinel());
                            }
                        } else {
                            raiseUndefinedVariable(context, chunk, name);
                        }
                    } else {
                        stack.push_back(value);
                    }
                } break;
                case OpCode::LOAD_GLOBAL_OR_JUMP: {
                    // Head of a possible package-qualified call: identical to
                    // LOAD_GLOBAL except that the final "Undefined variable"
                    // error is replaced by a jump to the package-call fallback.
                    if (ins.B >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    const std::string& name = chunk.names[ins.B];
                    ArrayOf value;
                    bool found = false;
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        Scope* globalScope = context->getGlobalScope();
                        found = globalScope != nullptr && globalScope->lookupVariable(name, value);
                    } else {
                        found = context->lookupVariable(name, value);
                    }
                    if (found) {
                        stack.push_back(value);
                        break;
                    }
                    if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                        stack.push_back(ArrayOf::empty());
                        break;
                    }
                    FunctionDef* fdef = nullptr;
                    if ((ins.flags & INST_FLAG_PREFER_DOTTED) != 0) {
                        // The dotted target (classdef constructor or static method,
                        // package function) wins over the head function.
                        if (ins.C >= chunk.names.size()) {
                            Error(L"Nelson:bytecode:nameIndexOutOfRange",
                                L"BytecodeVM: name index out of range.");
                        }
                        FunctionDef* dottedDef = nullptr;
                        if (lookupCachedFunction(ins.C, chunk.names[ins.C], dottedDef)) {
                            pc = ins.A;
                            break;
                        }
                    }
                    if (eval->lookupFunction(name, fdef)) {
                        try {
                            ArrayOfVector result = fdef->evaluateFunction(eval, {}, 1);
                            stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        } catch (const NelsonException& e) {
                            if (e.identifier() != L"Nelson:tooManyOutputs"
                                && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                && e.message()
                                    != Nelson::resolveMessageTemplate(
                                        L"Nelson:interpreter:notEnoughVarargout",
                                        L"Not enough outputs in varargout to satisfy call.")) {
                                throw;
                            }
                            (void)fdef->evaluateFunction(eval, {}, 0);
                            stack.push_back(noOutputSentinel());
                        }
                        break;
                    }
                    pc = ins.A;
                } break;
                case OpCode::ENSURE_DOTTED_FUNCTION: {
                    if (ins.A >= chunk.names.size() || ins.B >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    const std::string& dottedName = chunk.names[ins.A];
                    // Run-time counterpart of the compiler's isKnownDottedFunction:
                    // restricted to builtins and the path indexer on purpose (see
                    // CALL_CLASSDEF_MEMBER: classdef statics live in
                    // FunctionsInMemory and must keep their access rules).
                    FunctionDefPtr builtinFunction = nullptr;
                    bool known = BuiltInFunctionDefManager::getInstance()->find(
                        dottedName, builtinFunction);
                    if (!known) {
                        known = PathFunctionIndexerManager::getInstance()->functionFileExists(
                            dottedName);
                    }
                    if (!known) {
                        raiseUndefinedVariable(context, chunk, chunk.names[ins.B]);
                    }
                } break;
                case OpCode::LOAD_GLOBAL_COUNT_RANGE: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    ArrayOf value;
                    if (!context->lookupVariable(chunk.names[ins.A], value)) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) == 0) {
                            raiseUndefinedVariable(context, chunk, chunk.names[ins.A]);
                        }
                        value = ArrayOf::empty();
                    }
                    stack.push_back(ArrayOf::integerRange(1, 1, value.elementCount(), false));
                } break;
                case OpCode::LOAD_FIELD_TARGET_COUNT: {
                    ArrayOf value;
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        if (ins.A >= chunk.names.size()) {
                            Error(L"Nelson:bytecode:nameIndexOutOfRange",
                                L"BytecodeVM: name index out of range.");
                        }
                        if (!context->lookupVariable(chunk.names[ins.A], value)) {
                            value = ArrayOf::empty();
                        }
                    } else {
                        if (ins.A >= frame.locals.size()) {
                            Error(L"Nelson:bytecode:localSlotOutOfRange",
                                L"BytecodeVM: local slot out of range.");
                        }
                        if (isRuntimeAssignedLocalSlot(frame, ins.A)) {
                            value = frame.locals[ins.A];
                        } else if (ins.A < chunk.localNames.size()
                            && !chunk.localNames[ins.A].empty()
                            && context->lookupVariable(chunk.localNames[ins.A], value)) {
                        } else {
                            value = ArrayOf::empty();
                        }
                    }
                    // An undefined or empty target still expands to a single
                    // assignment slot: `[s.field] = value` creates a 1x1 struct.
                    indexType count = value.elementCount();
                    if (count == 0) {
                        count = 1;
                    }
                    stack.push_back(ArrayOf::integerRange(1, 1, count, false));
                } break;
                case OpCode::LOAD_ANS: {
                    ArrayOf value;
                    if (!tryGetAssignedLocalByName("ans", value)
                        && !context->lookupVariable("ans", value)) {
                        value = ArrayOf::empty();
                    }
                    stack.push_back(value);
                } break;
                case OpCode::STORE_GLOBAL: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    const std::string& name = chunk.names[ins.A];
                    ArrayOf value = pop();
                    bool inserted = false;
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        Scope* globalScope = context->getGlobalScope();
                        inserted = globalScope != nullptr
                            && globalScope->insertVariableWithoutNameValidation(name, value);
                    } else {
                        inserted = context->insertVariableWithoutNameValidation(name, value);
                    }
                    if (!inserted) {
                        Scope* globalScope = context->getGlobalScope();
                        bool isLockedGlobal = (ins.flags & INST_FLAG_GLOBAL) != 0
                            && globalScope != nullptr && globalScope->isLockedVariable(name);
                        if (isLockedGlobal || context->isLockedVariable(name)) {
                            Error(L"Nelson:interpreter:redefinePermanent",
                                L"Redefining permanent variable.");
                        }
                        Error(L"Nelson:interpreter:invalidVariableName",
                            L"Valid variable name expected.");
                    }
                } break;
                case OpCode::STORE_ANS: {
                    ArrayOf value = pop();
                    if (!isNoOutputSentinel(value)) {
                        setAssignedLocalByName("ans", value);
                        context->insertVariable("ans", value);
                    }
                    break;
                }
                case OpCode::DELETE_LOCAL:
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()) {
                        context->deleteVariable(chunk.localNames[ins.A]);
                    }
                    frame.locals[ins.A] = ArrayOf::empty();
                    markRuntimeUnassigned(frame, ins.A);
                    break;
                case OpCode::CLEAR_LOCAL:
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()) {
                        const std::string& name = chunk.localNames[ins.A];
                        if (isRuntimeAssignedLocalSlot(frame, ins.A)
                            && frame.locals[ins.A].isHandle()) {
                            context->insertVariableLocally(name, frame.locals[ins.A]);
                            ClearHandle(eval, context->getCurrentScope(), name);
                        }
                        context->deleteVariable(name);
                    }
                    frame.locals[ins.A] = ArrayOf::empty();
                    markRuntimeUnassigned(frame, ins.A);
                    break;
                case OpCode::POP:
                    (void)pop();
                    break;
                case OpCode::DUP:
                    if (stack.empty()) {
                        Error(L"Nelson:bytecode:stackUnderflow", L"BytecodeVM: stack underflow.");
                    }
                    stack.push_back(stack.back());
                    break;
                case OpCode::SWAP:
                    if (stack.size() < 2) {
                        Error(L"Nelson:bytecode:stackUnderflow", L"BytecodeVM: stack underflow.");
                    }
                    std::swap(stack[stack.size() - 1], stack[stack.size() - 2]);
                    break;
                case OpCode::SET_NAME:
                    if (stack.empty() || ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameOperandOutOfRange",
                            L"BytecodeVM: name operand out of range.");
                    }
                    if (chunk.names[ins.A].empty()
                        && (isCommaListSentinel(stack.back())
                            || isNoOutputSentinel(stack.back()))) {
                        break;
                    }
                    stack.back().name(chunk.names[ins.A]);
                    break;
                case OpCode::PUSH_END_CTX: {
                    if (ins.A >= stack.size()) {
                        Error(L"Nelson:bytecode:endContextStackUnderflow",
                            L"BytecodeVM: end context stack underflow.");
                    }
                    const ArrayOf& base = stack[stack.size() - 1 - ins.A];
                    frame.endContexts.push_back(EndContext {
                        base, static_cast<indexType>(ins.B), static_cast<size_t>(ins.C) });
                } break;
                case OpCode::PUSH_END_CTX_LOCAL: {
                    // end context for the in-place indexed-assignment opcodes:
                    // the base never transits through the stack, so read it
                    // from the local slot (seeded from the calling workspace
                    // exactly like the assignment opcode that follows will);
                    // an unassigned slot behaves as an empty base, matching
                    // LOAD_LOCAL with INST_FLAG_ALLOW_UNDEFINED.
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    ArrayOf base = ArrayOf::empty();
                    if (isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        base = frame.locals[ins.A];
                    }
                    frame.endContexts.push_back(EndContext {
                        base, static_cast<indexType>(ins.B), static_cast<size_t>(ins.C) });
                } break;
                case OpCode::POP_END_CTX:
                    if (!frame.endContexts.empty()) {
                        frame.endContexts.pop_back();
                    }
                    break;
                case OpCode::LOAD_END:
                    if (frame.endContexts.empty()) {
                        Error(L"Nelson:interpreter:illegalEnd", L"END keyword illegal!");
                    }
                    stack.push_back(eval->bytecodeEndReference(frame.endContexts.back().base,
                        frame.endContexts.back().index, frame.endContexts.back().count));
                    break;
                case OpCode::JUMP:
                    if (ins.A < pc) {
                        pollLoopBackedge();
                    }
                    pc = ins.A;
                    break;
                case OpCode::JUMP_IF_FALSE: {
                    ArrayOf condition = pop();
                    if (!conditionAsBoolean(eval, condition)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_TRUE: {
                    ArrayOf condition = pop();
                    if (conditionAsBoolean(eval, condition)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_FALSE_SCALAR: {
                    ArrayOf condition = pop();
                    if (!checkShortcutScalarCondition(eval, condition, true)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_TRUE_SCALAR: {
                    ArrayOf condition = pop();
                    if (checkShortcutScalarCondition(eval, condition, false)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_NOT_TABLE:
                    if (stack.empty() || !isTableDotIndexAssignTarget(eval, stack.back())) {
                        pc = ins.A;
                    }
                    break;
                case OpCode::JUMP_IF_NARGIN_CMP: {
                    if (Profiler::getInstance()->isOn()) {
                        // the generic condition would record the comparison
                        // operator on the current statement line
                        static const OpCode cmpOps[6] = { OpCode::OP_LT, OpCode::OP_LEQ,
                            OpCode::OP_GT, OpCode::OP_GEQ, OpCode::OP_EQ, OpCode::OP_NEQ };
                        profiledVoidOperation(
                            eval, profileName(cmpOps[ins.flags <= 5 ? ins.flags : 5]), []() { });
                    }
                    int narginValue = frame.nArgIn;
                    if (narginValue < 0) {
                        Scope* currentScope = context->getCurrentScope();
                        narginValue = currentScope != nullptr ? currentScope->getNargIn() : 0;
                    }
                    const int bound = static_cast<int>(ins.B);
                    bool condition = false;
                    switch (ins.flags) {
                    case NARGIN_CMP_LT:
                        condition = narginValue < bound;
                        break;
                    case NARGIN_CMP_LE:
                        condition = narginValue <= bound;
                        break;
                    case NARGIN_CMP_GT:
                        condition = narginValue > bound;
                        break;
                    case NARGIN_CMP_GE:
                        condition = narginValue >= bound;
                        break;
                    case NARGIN_CMP_EQ:
                        condition = narginValue == bound;
                        break;
                    default:
                        condition = narginValue != bound;
                        break;
                    }
                    if (!condition) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_ARG_PRED: {
                    // any doubt (profiling, shadowed predicate, missing base)
                    // falls through to the generic condition window that
                    // follows and keeps its exact observable behavior
                    const uint8_t predId = ins.flags & ARG_PRED_ID_MASK;
                    if (Profiler::getInstance()->isOn()
                        || !fusedArgPredBuiltinIsCanonical(eval, predId)) {
                        break;
                    }
                    const char* predName = predId == ARG_PRED_ISA ? "isa" : "isscalar";
                    ArrayOf shadowedVariable;
                    if (context->lookupVariable(predName, shadowedVariable)) {
                        break;
                    }
                    const ArrayOf* base = nullptr;
                    ArrayOf contextBase;
                    if (ins.B < frame.locals.size() && isRuntimeAssignedLocalSlot(frame, ins.B)) {
                        base = &frame.locals[ins.B];
                    } else if (ins.B < chunk.localNames.size() && !chunk.localNames[ins.B].empty()
                        && context->lookupVariable(chunk.localNames[ins.B], contextBase)) {
                        base = &contextBase;
                    } else {
                        break;
                    }
                    bool predicate;
                    if (predId == ARG_PRED_ISA) {
                        predicate = nativeIsaCheck(
                            *base, std::get<std::string>(chunk.constants.get(ins.C)));
                    } else {
                        predicate = base->isScalar();
                    }
                    const bool jumpTaken
                        = (ins.flags & ARG_PRED_JUMP_ON_TRUE) != 0 ? predicate : !predicate;
                    if (jumpTaken) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    } else {
                        pc += (ins.flags >> ARG_PRED_WINDOW_SHIFT) & ARG_PRED_WINDOW_MASK;
                    }
                } break;
                case OpCode::CHECK_SCALAR_AND: {
                    ArrayOf condition = pop();
                    stack.push_back(ArrayOf::logicalScalar(
                        checkShortcutScalarCondition(eval, condition, true)));
                } break;
                case OpCode::CHECK_SCALAR_OR: {
                    ArrayOf condition = pop();
                    stack.push_back(ArrayOf::logicalScalar(
                        checkShortcutScalarCondition(eval, condition, false)));
                } break;
                case OpCode::CASE_MATCH: {
                    ArrayOf caseValue = pop();
                    ArrayOf switchValue = pop();
                    // A class or handle switch value cannot be matched directly
                    // (it is a reference type); dispatch to its `char` overload
                    // first, so `switch obj` against 'on'/'off' case labels works
                    // whenever the class defines char() (e.g. an on/off state).
                    if (switchValue.isClassType() || switchValue.isHandle()) {
                        bool wasFound = false;
                        ArrayOf converted
                            = callOperatorOverload(eval, switchValue, "char", wasFound);
                        if (wasFound) {
                            switchValue = converted;
                        }
                    }
                    if (pc < chunk.code.size() && chunk.code[pc].op == OpCode::JUMP_IF_FALSE) {
                        const Instruction& jump = chunk.code[pc++];
                        if (!switchValue.testForCaseMatch(caseValue)) {
                            if (jump.A < pc) {
                                pollLoopBackedge();
                            }
                            pc = jump.A;
                        }
                        break;
                    }
                    stack.push_back(
                        ArrayOf::logicalScalar(switchValue.testForCaseMatch(caseValue)));
                } break;
                case OpCode::TRY_BEGIN:
                    frame.exceptionFrames.push_back(ExceptionFrame { pc, ins.A });
                    break;
                case OpCode::TRY_END:
                    if (!frame.exceptionFrames.empty()) {
                        frame.exceptionFrames.pop_back();
                    }
                    break;
                case OpCode::CATCH_BEGIN:
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        if (ins.A >= chunk.names.size()) {
                            Error(L"Nelson:bytecode:nameIndexOutOfRange",
                                L"BytecodeVM: name index out of range.");
                        }
                        context->insertVariable(chunk.names[ins.A],
                            NelsonExceptionToArrayOf(eval->getLastErrorException()));
                        break;
                    }
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    frame.locals[ins.A] = NelsonExceptionToArrayOf(eval->getLastErrorException());
                    markRuntimeAssigned(frame, ins.A);
                    break;
                case OpCode::WHILE_CHECK: {
                    ArrayOf condition = pop();
                    if (!conditionAsBoolean(eval, condition)) {
                        pc = ins.A;
                    }
                } break;
                case OpCode::FOR_INIT: {
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        initGenericLoopGlobal(
                            context, chunk, frame, ins.A, ins.B, pop(), pc, ins.C);
                    } else {
                        initGenericLoop(frame, ins.A, ins.B, pop(), pc, ins.C);
                    }
                } break;
                case OpCode::FOR_STEP: {
                    uint32_t beforeStepPC = pc;
                    if ((ins.flags & INST_FLAG_GLOBAL) != 0) {
                        stepGenericLoopGlobal(context, chunk, frame, ins.A, ins.B, pc, ins.C);
                    } else {
                        stepGenericLoop(frame, ins.A, ins.B, pc, ins.C);
                    }
                    if (pc < beforeStepPC) {
                        pollLoopBackedge();
                    }
                } break;
                case OpCode::FOR_INIT_DBL: {
                    bool isGlobal = (ins.flags & INST_FLAG_GLOBAL) != 0;
                    if (ins.A >= frame.dblLoops.size()
                        || (!isGlobal && ins.B >= frame.locals.size())
                        || (isGlobal && ins.B >= chunk.names.size())) {
                        Error(L"Nelson:bytecode:doubleForLoopOperandOutOfRange",
                            L"BytecodeVM: double for-loop operand out of range.");
                    }
                    ArrayOf indexSet = pop();
                    if (!indexSet.isRowVector() || !indexSet.isDoubleType(true)) {
                        frame.dblLoops[ins.A].dataPtr = nullptr;
                        if (isGlobal) {
                            initGenericLoopGlobal(
                                context, chunk, frame, ins.A, ins.B, indexSet, pc, ins.C);
                        } else {
                            initGenericLoop(frame, ins.A, ins.B, indexSet, pc, ins.C);
                        }
                        break;
                    }
                    DoubleLoopState& state = frame.dblLoops[ins.A];
                    state.indexSet = indexSet;
                    state.dataPtr = static_cast<const double*>(state.indexSet.dataPointer());
                    state.current = 0;
                    state.count = state.indexSet.elementCount();
                    if (state.count == 0) {
                        pc = ins.C;
                    } else if (isGlobal) {
                        context->insertVariable(
                            chunk.names[ins.B], ArrayOf::doubleScalar(state.dataPtr[0]));
                    } else {
                        setLocalDouble(frame.locals, ins.B, state.dataPtr[0]);
                        markRuntimeAssigned(frame, ins.B);
                    }
                } break;
                case OpCode::FOR_STEP_DBL: {
                    bool isGlobal = (ins.flags & INST_FLAG_GLOBAL) != 0;
                    if (ins.A >= frame.dblLoops.size()
                        || (!isGlobal && ins.B >= frame.locals.size())
                        || (isGlobal && ins.B >= chunk.names.size())) {
                        Error(L"Nelson:bytecode:doubleForLoopOperandOutOfRange",
                            L"BytecodeVM: double for-loop operand out of range.");
                    }
                    DoubleLoopState& state = frame.dblLoops[ins.A];
                    if (state.dataPtr == nullptr) {
                        uint32_t beforeStepPC = pc;
                        if (isGlobal) {
                            stepGenericLoopGlobal(context, chunk, frame, ins.A, ins.B, pc, ins.C);
                        } else {
                            stepGenericLoop(frame, ins.A, ins.B, pc, ins.C);
                        }
                        if (pc < beforeStepPC) {
                            pollLoopBackedge();
                        }
                        break;
                    }
                    ++state.current;
                    if (state.current < state.count) {
                        if (isGlobal) {
                            context->insertVariable(chunk.names[ins.B],
                                ArrayOf::doubleScalar(state.dataPtr[state.current]));
                        } else {
                            setLocalDouble(frame.locals, ins.B, state.dataPtr[state.current]);
                            markRuntimeAssigned(frame, ins.B);
                        }
                        if (ins.C < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.C;
                    }
                } break;
                case OpCode::FOR_INIT_RANGE: {
                    bool isGlobal = (ins.flags & INST_FLAG_GLOBAL) != 0;
                    if (ins.A >= frame.rangeLoops.size()
                        || (!isGlobal && ins.B >= frame.locals.size())
                        || (isGlobal && ins.B >= chunk.names.size())) {
                        Error(L"Nelson:bytecode:rangeForLoopOperandOutOfRange",
                            L"BytecodeVM: range for-loop operand out of range.");
                    }
                    RangeLoopState& state = frame.rangeLoops[ins.A];
                    state.stop = pop().asDoubleScalar();
                    state.step
                        = (ins.flags & INST_FLAG_RANGE_HAS_STEP) ? pop().asDoubleScalar() : 1.0;
                    state.start = pop().asDoubleScalar();
                    state.current = state.start;
                    profiledVoidOperation(eval, COLON_OPERATOR_STR, []() { });
                    if (state.step == 0.0 || (state.step > 0.0 && state.current > state.stop)
                        || (state.step < 0.0 && state.current < state.stop)) {
                        pc = ins.C;
                    } else if (isGlobal) {
                        context->insertVariable(
                            chunk.names[ins.B], ArrayOf::doubleScalar(state.current));
                    } else {
                        setLocalDouble(frame.locals, ins.B, state.current);
                        markRuntimeAssigned(frame, ins.B);
                        if (ins.A < scalarRanges.programs.size()) {
                            scalarRanges.programs[ins.A].enabled = false;
                        }
                        if (worthPlanningScalarRange(state)) {
                            beginScalarRangeLoop(scalarRanges, eval, frame, chunk, context, ins, pc,
                                loopBackedgeCounter, scalarRangeBoundary);
                        }
                    }
                } break;
                case OpCode::FOR_STEP_RANGE: {
                    if (advanceRangeLoop(frame, chunk, context, ins)) {
                        if (ins.C < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.C;
                        if (ins.A < scalarRanges.programs.size()
                            && scalarRanges.programs[ins.A].enabled) {
                            continueScalarRangeLoop(scalarRanges.programs[ins.A], eval, frame,
                                chunk, context, pc, loopBackedgeCounter, scalarRangeBoundary);
                        }
                    }
                } break;
                case OpCode::BREAK:
                    eval->setState(NLS_STATE_BREAK);
                    pc = ins.A;
                    eval->resetState();
                    break;
                case OpCode::CONTINUE:
                    eval->setState(NLS_STATE_CONTINUE);
                    if (ins.A < pc) {
                        pollLoopBackedge();
                    }
                    pc = ins.A;
                    eval->resetState();
                    break;
                case OpCode::OP_PLUS:
                case OpCode::OP_MINUS:
                case OpCode::OP_MTIMES:
                case OpCode::OP_TIMES:
                case OpCode::OP_MRDIV:
                case OpCode::OP_RDIV:
                case OpCode::OP_MLDIV:
                case OpCode::OP_LDIV:
                case OpCode::OP_MPOWER:
                case OpCode::OP_POWER:
                case OpCode::OP_LT:
                case OpCode::OP_LEQ:
                case OpCode::OP_GT:
                case OpCode::OP_GEQ:
                case OpCode::OP_EQ:
                case OpCode::OP_NEQ:
                case OpCode::OP_AND:
                case OpCode::OP_OR:
                case OpCode::OP_COLON_UNIT: {
                    ArrayOf b = pop();
                    ArrayOf a = pop();
                    bool comparison = false;
                    if (tryScalarDoubleComparison(ins.op, a, b, comparison)
                        && pc < chunk.code.size()) {
                        const Instruction& branch = chunk.code[pc];
                        if (applyImmediateBranch(branch, comparison)) {
                            break;
                        }
                    }
                    stack.push_back(binaryOperator(eval, ins.op, a, b));
                } break;
                case OpCode::OP_COLON: {
                    ArrayOf c = pop();
                    ArrayOf b = pop();
                    ArrayOf a = pop();
                    stack.push_back(ternaryOperator(eval, ins.op, a, b, c));
                } break;
                case OpCode::OP_UPLUS:
                case OpCode::OP_UMINUS:
                case OpCode::OP_NOT:
                case OpCode::OP_TRANSPOSE:
                case OpCode::OP_DOT_TRANSPOSE: {
                    ArrayOf a = pop();
                    stack.push_back(unaryOperator(eval, ins.op, a));
                } break;
                case OpCode::SUPER_CALL: {
                    // obj@Super(...) / method@Super(...): pop the operands
                    // (constructor: [obj, 'Super', args...]; method: ['Super',
                    // 'method', args...]), expanding comma lists exactly as
                    // CALL_NAMED does, then dispatch to the engine and push the
                    // nout results.
                    const uint16_t superNout = ins.C;
                    const bool isConstructor = (ins.flags & INST_FLAG_SUPER_CONSTRUCTOR) != 0;
                    ArrayOfVector superArgs = expandCommaLists(popVector(ins.B));
                    ArrayOfVector superResults = evaluateClassdefSuperCall(
                        eval, isConstructor, superArgs, static_cast<int>(superNout));
                    for (uint16_t i = 0; i < superNout; ++i) {
                        stack.push_back(
                            i < superResults.size() ? superResults[i] : ArrayOf::empty());
                    }
                } break;
                case OpCode::CALL_NAMED: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    const std::string& name = chunk.names[ins.A];
                    // Native classdef engine operations performed directly in the VM
                    if (name.size() >= 17 && name[0] == '_' && name[2] == 'c') {
                        if (name == "__classdef_value__" || name == "__classdef_handle__") {
                            ArrayOfVector constructArgs = expandCommaLists(popVector(ins.B));
                            ArrayOf instance
                                = createClassdefInstance(name[11] == 'h', constructArgs);
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i == 0 ? instance : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_property_default__") {
                            ArrayOfVector defaultArgs = expandCommaLists(popVector(ins.B));
                            ArrayOfVector defaultResults
                                = evaluateClassdefPropertyDefault(eval, defaultArgs);
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < defaultResults.size() ? defaultResults[i]
                                                                          : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_handle_set_stored__") {
                            ArrayOfVector storedArgs = expandCommaLists(popVector(ins.B));
                            evaluateClassdefSetStored(eval, storedArgs);
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_value_set_stored__") {
                            ArrayOfVector storedArgs = expandCommaLists(popVector(ins.B));
                            ArrayOfVector storedResults
                                = evaluateClassdefValueSetStored(eval, storedArgs);
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(
                                    i < storedResults.size() ? storedResults[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_handle_element__"
                            || name == "__classdef_handle_struct__"
                            || name == "__classdef_handle_delete__") {
                            ArrayOfVector handleArgs = expandCommaLists(popVector(ins.B));
                            ArrayOfVector handleResults;
                            if (name[18] == 'e') {
                                handleResults = evaluateClassdefHandleElement(handleArgs);
                            } else if (name[18] == 's') {
                                handleResults = evaluateClassdefHandleStruct(handleArgs);
                            } else {
                                handleResults = evaluateClassdefHandleDelete(eval, handleArgs);
                            }
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(
                                    i < handleResults.size() ? handleResults[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_handle_isvalid__") {
                            ArrayOfVector r
                                = evaluateClassdefHandleIsValid(expandCommaLists(popVector(ins.B)));
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < r.size() ? r[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_copy__") {
                            ArrayOfVector r
                                = evaluateClassdefCopy(eval, expandCommaLists(popVector(ins.B)));
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < r.size() ? r[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_handle_get__") {
                            ArrayOfVector r = evaluateClassdefHandleGet(
                                eval, expandCommaLists(popVector(ins.B)));
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < r.size() ? r[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_handle_set__") {
                            evaluateClassdefHandleSet(eval, expandCommaLists(popVector(ins.B)));
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_add_dynamic_property__"
                            || name == "__classdef_remove_dynamic_property__") {
                            ArrayOfVector dynArgs = expandCommaLists(popVector(ins.B));
                            ArrayOfVector r = name[11] == 'a'
                                ? evaluateClassdefAddDynamicProperty(eval, dynArgs)
                                : evaluateClassdefRemoveDynamicProperty(eval, dynArgs);
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < r.size() ? r[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_invalid_handle__") {
                            ArrayOfVector r
                                = evaluateClassdefInvalidHandle(expandCommaLists(popVector(ins.B)));
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < r.size() ? r[i] : ArrayOf::empty());
                            }
                            break;
                        }
                        if (name == "__classdef_weak_reference_new__"
                            || name == "__classdef_weak_reference_get__"
                            || name == "__classdef_weak_reference_valid_get__"
                            || name == "__classdef_weak_reference_set__") {
                            ArrayOfVector r = evaluateClassdefWeakReference(
                                name, expandCommaLists(popVector(ins.B)));
                            for (uint16_t i = 0; i < ins.C; ++i) {
                                stack.push_back(i < r.size() ? r[i] : ArrayOf::empty());
                            }
                            break;
                        }
                    }
                    if (tryEvaluateNamedScalarBuiltinFromStack(ins, name)) {
                        break;
                    }
                    // Single shadow check per call: a runtime variable of the
                    // same name takes precedence over the function. Nothing
                    // between here and the dispatch below can create one.
                    ArrayOf variable = ArrayOf::doubleScalar(0.);
                    bool hasVariable = tryGetAssignedLocalByNameIndex(ins.A, variable);
                    if (!hasVariable) {
                        hasVariable = context->lookupVariable(name, variable);
                    }
                    ArrayOfVector args;
                    if (ins.B == 1) {
                        ArrayOf singleArg = pop();
                        if (!isCommaListSentinel(singleArg)) {
                            if ((ins.flags & INST_FLAG_PRINT) == 0 && ins.C == 1
                                && name == "string") {
                                FunctionDef* fdef = nullptr;
                                ArrayOf fastResult;
                                if (!hasVariable && eval->lookupFunction(name, fdef)
                                    && isResolvedBuiltinStringFunction(name, fdef)
                                    && tryEvaluateBuiltinStringFast(singleArg, fastResult)) {
                                    stack.push_back(fastResult);
                                    break;
                                }
                            }
                            if (frame.skipDebugStatements && (ins.flags & INST_FLAG_PRINT) == 0
                                && name == chunk.functionName && chunk.selfFunction != nullptr
                                && !chunk.selfFunction->isOverload()) {
                                FunctionDef* fdef = chunk.selfFunction;
                                BytecodeChunk* calleeChunk = nullptr;
                                if (canDirectSelfBytecodeCall(
                                        eval, chunk, fdef, &singleArg, 1, ins.C, calleeChunk)
                                    && calleeChunk != nullptr && ins.C == 1) {
                                    ArrayOf value = executeFunctionOneArgOneOutput(
                                        eval, *calleeChunk, singleArg, false);
                                    State state(eval->getState());
                                    if (state < NLS_STATE_QUIT) {
                                        eval->resetState();
                                    }
                                    if (state == NLS_STATE_ABORT || state == NLS_STATE_QUIT
                                        || state == NLS_STATE_FORCE_QUIT) {
                                        value = ArrayOf::empty();
                                    }
                                    value.name("");
                                    stack.push_back(value);
                                    break;
                                }
                            }
                            if (!hasVariable && (ins.flags & INST_FLAG_PRINT) == 0
                                && name == chunk.functionName && chunk.selfFunction != nullptr
                                && !chunk.selfFunction->isOverload()) {
                                FunctionDef* fdef = chunk.selfFunction;
                                bool syncCallerContext = needsCallerContextSync(fdef);
                                bool refreshCallerContext = needsCallerContextRefresh(fdef);
                                BytecodeChunk* calleeChunk = nullptr;
                                if (!syncCallerContext && !refreshCallerContext
                                    && canDirectSelfBytecodeCall(
                                        eval, chunk, fdef, &singleArg, 1, ins.C, calleeChunk)
                                    && calleeChunk != nullptr && ins.C == 1) {
                                    ArrayOf value = executeFunctionOneArgOneOutput(
                                        eval, *calleeChunk, singleArg, false);
                                    State state(eval->getState());
                                    if (state < NLS_STATE_QUIT) {
                                        eval->resetState();
                                    }
                                    if (state == NLS_STATE_ABORT || state == NLS_STATE_QUIT
                                        || state == NLS_STATE_FORCE_QUIT) {
                                        value = ArrayOf::empty();
                                    }
                                    value.name("");
                                    stack.push_back(value);
                                    break;
                                }
                            }
                            args.push_back(singleArg);
                        } else {
                            args.push_back(singleArg);
                            args = expandCommaLists(args);
                        }
                    } else {
                        args = expandCommaLists(popVector(ins.B));
                    }
                    if (hasVariable && variable.isFunctionHandle()) {
                        FunctionDef* handleDef = functionDefFromHandle(variable);
                        bool syncCallerContext = needsCallerContextSync(handleDef);
                        bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                        ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                        if (syncCallerContext) {
                            syncAssignedLocalsToContext();
                        }
                        if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0) {
                            ArrayOfVector result;
                            size_t clearEpoch = context->getVariablesClearedEpoch();
                            bool hasOutput = false;
                            result = callFunctionHandleAndDetectOutput(
                                eval, variable, args, 0, hasOutput);
                            refreshAfterVariableClear(clearEpoch);
                            if (refreshCallerContext) {
                                refreshLocalsFromContext();
                            }
                            if (result.empty() && !hasOutput
                                && (handleDef != nullptr
                                    && (handleDef->type() == NLS_ANONYMOUS_MACRO_FUNCTION
                                        || shouldRetrySilentVarargoutCall(handleDef)))) {
                                clearEpoch = context->getVariablesClearedEpoch();
                                try {
                                    result = callFunctionHandle(eval, variable, args, 1);
                                } catch (const NelsonException& e) {
                                    if (e.identifier() != L"Nelson:tooManyOutputs"
                                        && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                        && e.message()
                                            != Nelson::resolveMessageTemplate(
                                                L"Nelson:nargoutchk:noOutputWithTwoInputs",
                                                L"No output arguments are allowed if only two "
                                                L"input arguments.")
                                        && e.message()
                                            != Nelson::resolveMessageTemplate(
                                                L"Nelson:interpreter:notEnoughVarargout",
                                                L"Not enough outputs in varargout to satisfy "
                                                L"call.")) {
                                        throw;
                                    }
                                    result = {};
                                }
                                refreshAfterVariableClear(clearEpoch);
                                if (refreshCallerContext) {
                                    refreshLocalsFromContext();
                                }
                            }
                            if (!result.empty()) {
                                context->insertVariable("ans", result[0]);
                                for (size_t k = 0; k < result.size(); ++k) {
                                    eval->display(result[k], "ans", false, true);
                                }
                            }
                            break;
                        }
                        uint16_t nout = ins.C;
                        ArrayOfVector result;
                        size_t clearEpoch = context->getVariablesClearedEpoch();
                        try {
                            result
                                = callFunctionHandle(eval, variable, args, static_cast<int>(nout));
                        } catch (const NelsonException& e) {
                            if (!(ins.flags & INST_FLAG_PRINT) || nout != 1
                                || (e.identifier() != L"Nelson:tooManyOutputs"
                                    && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:nargoutchk:noOutputWithTwoInputs",
                                            L"No output arguments are allowed if only two input "
                                            L"arguments.")
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:interpreter:notEnoughVarargout",
                                            L"Not enough outputs in varargout to satisfy call."))) {
                                throw;
                            }
                            nout = 0;
                            result = callFunctionHandle(eval, variable, args, 0);
                        }
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                        for (uint16_t k = 0; k < nout; ++k) {
                            if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 1
                                && result.empty()) {
                                stack.push_back(noOutputSentinel());
                            } else {
                                ArrayOf value = k < result.size() ? result[k] : ArrayOf::empty();
                                if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 1
                                    && value.name().empty()) {
                                    value.name("ans");
                                }
                                stack.push_back(value);
                            }
                        }
                        if (ins.C == 1 && nout == 0) {
                            stack.push_back(noOutputSentinel());
                        }
                        break;
                    }
                    if (hasVariable && !args.empty()
                        && (variable.isClassType() || variable.isHandle())) {
                        // indexing a class/handle object held in a non-local
                        // variable: dispatch to subsref, honoring the requested
                        // number of outputs (as the local-variable path does)
                        stringVector subtypes;
                        subtypes.push_back("()");
                        ArrayOfVector subsindices;
                        subsindices.push_back(classIndexCell(args));
                        bool haveFunction = false;
                        int nout = ins.C > 1 ? static_cast<int>(ins.C) : 1;
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            variable, subtypes, subsindices, haveFunction, nout);
                        if (haveFunction) {
                            if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0) {
                                if (!result.empty()) {
                                    context->insertVariable("ans", result[0]);
                                    for (size_t k = 0; k < result.size(); ++k) {
                                        eval->display(result[k], "ans", false, true);
                                    }
                                }
                            } else {
                                for (uint16_t k = 0; k < ins.C; ++k) {
                                    stack.push_back(
                                        k < result.size() ? result[k] : ArrayOf::empty());
                                }
                            }
                            break;
                        }
                        // no subsref overload: fall through to the raw subset paths
                    }
                    if (hasVariable) {
                        resolveObjectIndices(eval, args);
                    }
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0 && hasVariable
                        && !args.empty()) {
                        ArrayOf result;
                        if (args.size() == 1) {
                            result = variable.vectorSubset(args[0]);
                        } else {
                            result = variable.nDimSubset(args);
                        }
                        context->insertVariable("ans", result);
                        eval->display(result, "ans", false, true);
                        break;
                    }
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0 && hasVariable
                        && args.empty()) {
                        eval->display(variable, name, false, false);
                        break;
                    }
                    if (ins.C == 0 && args.empty() && hasVariable) {
                        break;
                    }
                    if (ins.C == 1 && hasVariable) {
                        if (args.empty()) {
                            stack.push_back(variable);
                        } else if (args.size() == 1) {
                            stack.push_back(variable.vectorSubset(args[0]));
                        } else {
                            stack.push_back(variable.nDimSubset(args));
                        }
                        break;
                    }
                    FunctionDef* fdef = nullptr;
                    if (!hasVariable && name == chunk.functionName && chunk.selfFunction != nullptr
                        && !chunk.selfFunction->isOverload()) {
                        fdef = chunk.selfFunction;
                    } else if (!lookupCachedFunction(ins.A, name, fdef)
                        && !tryReloadStaleClassdefMemberFunction(name, fdef)) {
                        // No function of that name: a method of the dominant
                        // object argument, then of any other object argument.
                        const bool printMethodResult
                            = (ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0;
                        const int requestedOutputs
                            = printMethodResult ? 1 : static_cast<int>(ins.C);
                        const size_t dominant = overloadDispatchArgumentIndex(args, name);
                        ArrayOfVector result;
                        bool invokedObjectMethod = dominant > 0
                            && eval->bytecodeInvokeObjectMethodIfExistsWithInputs(
                                args[dominant], name, args, requestedOutputs, result);
                        if (!invokedObjectMethod && !args.empty()
                            && (args[0].isHandle() || args[0].isClassType())) {
                            ArrayOfVector methodArgs;
                            for (size_t k = 1; k < args.size(); ++k) {
                                methodArgs.push_back(args[k]);
                            }
                            invokedObjectMethod = eval->bytecodeInvokeObjectMethodIfExists(
                                args[0], name, methodArgs, requestedOutputs, result);
                        }
                        for (size_t receiverIndex = 1;
                            !invokedObjectMethod && receiverIndex < args.size(); ++receiverIndex) {
                            const ArrayOf& receiver = args[receiverIndex];
                            if (receiverIndex != dominant
                                && (receiver.isHandle() || receiver.isClassType())) {
                                invokedObjectMethod
                                    = eval->bytecodeInvokeObjectMethodIfExistsWithInputs(
                                        receiver, name, args, requestedOutputs, result);
                            }
                        }
                        if (invokedObjectMethod) {
                            if (printMethodResult) {
                                if (!result.empty()) {
                                    context->insertVariable("ans", result[0]);
                                    for (size_t k = 0; k < result.size(); ++k) {
                                        eval->display(result[k], "ans", false, true);
                                    }
                                }
                            } else {
                                for (uint16_t k = 0; k < ins.C; ++k) {
                                    stack.push_back(
                                        k < result.size() ? result[k] : ArrayOf::empty());
                                }
                            }
                            break;
                        }
                        Error("Nelson:UndefinedFunction",
                            formatI18nMessage(
                                Nelson::resolveMessageTemplate(
                                    "Nelson:UndefinedFunction:undefinedVariableFunctionValue",
                                    "Undefined variable or function: %s"),
                                name));
                    }
                    // Inside a classdef method, an unqualified call dispatches on its arguments.
                    // If it resolved to the method being executed (a method whose name shadows a
                    // builtin) but the leading argument is not of that class, use the builtin
                    // (e.g. isa(aDouble,...)). This stops a builtin-shadowing method from
                    // recursing forever. Only direct self-recursion inside a class method is
                    // affected.
                    if (fdef != nullptr && fdef == chunk.selfFunction
                        && !eval->getClassdefAccessContext().empty()) {
                        const std::string& enclosingClass = eval->getClassdefAccessContext();
                        bool leadingArgIsClass = !args.empty()
                            && (args[0].isClassType() || args[0].isHandle())
                            && classdefIsA(args[0].isHandle() ? args[0].handleClassName()
                                                              : args[0].classType(),
                                enclosingClass);
                        if (!leadingArgIsClass) {
                            FunctionDefPtr builtinDef = nullptr;
                            if (BuiltInFunctionDefManager::getInstance()->find(name, builtinDef)
                                && builtinDef != nullptr && builtinDef != fdef) {
                                fdef = builtinDef;
                            }
                        }
                    }
                    // Converter dispatch: TargetClass(obj), where obj's class defines a method
                    // named TargetClass, invokes that method rather than the constructor. A static
                    // call (Class.method(obj, ...)) is never a conversion; probing its dotted name
                    // as a class would stat every path directory.
                    if (fdef != nullptr && !args.empty()
                        && (args[0].isClassType() || args[0].isHandle())
                        && !isClassdefStaticMethodName(name, eval->getClassdefAccessContext())) {
                        const std::string argClass
                            = args[0].isHandle() ? args[0].handleClassName() : args[0].classType();
                        // The argument's own method `name` wins over the same-named constructor.
                        // The method may be in the class body or in an external @class/name.m
                        // file, so resolve through the method resolver (which walks the path) as
                        // well as the in-body definition check.
                        std::string converterResolvedName;
                        const bool argClassHasConverter = !argClass.empty() && argClass != name
                            && classdefLoadClass(name) && classdefLoadClass(argClass)
                            && (classdefHasMethodDefinition(argClass, name)
                                || classdefResolveMethodFunction(argClass, name,
                                    converterResolvedName, eval->getClassdefAccessContext()));
                        if (argClassHasConverter) {
                            ArrayOfVector converterArgs;
                            for (size_t k = 1; k < args.size(); ++k) {
                                converterArgs.push_back(args[k]);
                            }
                            ArrayOfVector converterResult;
                            const int converterOutputs = ins.C > 0 ? static_cast<int>(ins.C) : 1;
                            if (eval->bytecodeInvokeObjectMethodIfExists(args[0], name,
                                    converterArgs, converterOutputs, converterResult)) {
                                for (uint16_t i = 0; i < ins.C; ++i) {
                                    stack.push_back(i < converterResult.size() ? converterResult[i]
                                                                               : ArrayOf::empty());
                                }
                                break;
                            }
                        }
                    }
                    if ((ins.flags & INST_FLAG_PRINT) == 0 && ins.C == 1
                        && isResolvedBuiltinStringFunction(name, fdef) && args.empty()) {
                        stack.push_back(ArrayOf::stringArray(std::string()));
                        break;
                    }
                    if ((ins.flags & INST_FLAG_PRINT) == 0 && ins.C == 1) {
                        ScalarBuiltinFastPath fastPath = scalarBuiltinFastPathForName(
                            name, static_cast<uint16_t>(args.size()));
                        ArrayOf inlineResult;
                        double scalarBinaryValue = 0;
                        if (tryEvaluateScalarBuiltinFastPath(
                                fastPath, fdef, args, scalarBinaryValue)) {
                            if (pc + 2 < chunk.code.size()) {
                                const Instruction& zeroLoad = chunk.code[pc];
                                const Instruction& comparison = chunk.code[pc + 1];
                                const Instruction& branch = chunk.code[pc + 2];
                                bool comparisonValue = false;
                                if (zeroLoad.op == OpCode::LOAD_CONST
                                    && isCachedScalarDoubleZeroConstant(zeroLoad.A)
                                    && tryCompareScalarDoubleWithZero(
                                        comparison.op, scalarBinaryValue, comparisonValue)
                                    && (branch.op == OpCode::WHILE_CHECK
                                        || branch.op == OpCode::JUMP_IF_FALSE
                                        || branch.op == OpCode::JUMP_IF_FALSE_SCALAR
                                        || branch.op == OpCode::JUMP_IF_TRUE
                                        || branch.op == OpCode::JUMP_IF_TRUE_SCALAR)) {
                                    pc += 2;
                                    applyImmediateBranch(branch, comparisonValue);
                                    break;
                                }
                            }
                            stack.push_back(ArrayOf::doubleScalar(scalarBinaryValue));
                            break;
                        }
                        if (tryEvaluateScalarBuiltinFastPath(fastPath, fdef, args, inlineResult)) {
                            stack.push_back(inlineResult);
                            break;
                        }
                        BytecodeChunk* leafChunk = nullptr;
                        if ((leafChunk != nullptr
                                || canDirectLeafMacroBytecodeCall(
                                    eval, fdef, args, ins.C, leafChunk))
                            && leafChunk != nullptr) {
                            // direct leaf call: no scope pushed for the callee
                            ArrayOf value
                                = executeFunctionOneArgOneOutput(eval, *leafChunk, args[0], false);
                            State state(eval->getState());
                            if (state < NLS_STATE_QUIT) {
                                eval->resetState();
                            }
                            if (state == NLS_STATE_ABORT || state == NLS_STATE_QUIT
                                || state == NLS_STATE_FORCE_QUIT) {
                                value = ArrayOf::empty();
                            }
                            value.name("");
                            stack.push_back(value);
                            break;
                        }
                    }
                    bool syncCallerContext = needsCallerContextSync(fdef);
                    bool refreshCallerContext = needsCallerContextRefresh(fdef);
                    ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0
                        && displayImplicitObjectOverloadCall(
                            eval, name, args, refreshCallerContext, refreshLocalsFromContext)) {
                        break;
                    }
                    uint16_t nout = ins.C;
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && nout == 0
                        && (fdef->type() == NLS_MACRO_FUNCTION
                            || fdef->type() == NLS_ANONYMOUS_MACRO_FUNCTION)
                        && fdef->outputArgCount() > 0) {
                        nout = 1;
                    }
                    ArrayOfVector result;
                    if ((ins.flags & INST_FLAG_PRINT) == 0 && !syncCallerContext
                        && !refreshCallerContext
                        && tryEvaluateDirectSelfBytecode(fdef, args, nout, result)) {
                        for (uint16_t k = 0; k < nout; ++k) {
                            stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                        }
                        if (ins.C == 1 && nout == 0) {
                            stack.push_back(noOutputSentinel());
                        }
                        break;
                    }
                    bool alreadyEvaluated = false;
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0 && nout == 0
                        && shouldRetrySilentVarargoutCall(fdef)) {
                        bool hasOutput = false;
                        size_t clearEpoch = context->getVariablesClearedEpoch();
                        result = evaluateFunctionAndDetectOutput(eval, fdef, args, 0, hasOutput);
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                        if (result.empty() && !hasOutput) {
                            clearEpoch = context->getVariablesClearedEpoch();
                            try {
                                result = fdef->evaluateFunction(eval, args, 1);
                            } catch (const NelsonException& e) {
                                if (e.identifier() != L"Nelson:tooManyOutputs"
                                    && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:nargoutchk:noOutputWithTwoInputs",
                                            L"No output arguments are allowed if only two input "
                                            L"arguments.")
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:interpreter:notEnoughVarargout",
                                            L"Not enough outputs in varargout to satisfy call.")) {
                                    throw;
                                }
                                result = {};
                            }
                            refreshAfterVariableClear(clearEpoch);
                            if (refreshCallerContext) {
                                refreshLocalsFromContext();
                            }
                        }
                        alreadyEvaluated = true;
                    }
                    if (!alreadyEvaluated && (ins.flags & INST_FLAG_PRINT) != 0 && nout == 1
                        && fdef->outputArgCount() == 0) {
                        size_t clearEpoch = context->getVariablesClearedEpoch();
                        result = fdef->evaluateFunction(eval, args, 0);
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                        stack.push_back(noOutputSentinel());
                        break;
                    }
                    if (!alreadyEvaluated) {
                        size_t clearEpoch = context->getVariablesClearedEpoch();
                        try {
                            result = fdef->evaluateFunction(eval, args, static_cast<int>(nout));
                        } catch (const NelsonException& e) {
                            if (!(ins.flags & INST_FLAG_PRINT) || nout != 1
                                || (e.identifier() != L"Nelson:tooManyOutputs"
                                    && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:nargoutchk:noOutputWithTwoInputs",
                                            L"No output arguments are allowed if only two input "
                                            L"arguments.")
                                    && e.message()
                                        != Nelson::resolveMessageTemplate(
                                            L"Nelson:interpreter:notEnoughVarargout",
                                            L"Not enough outputs in varargout to satisfy call."))) {
                                throw;
                            }
                            nout = 0;
                            result = fdef->evaluateFunction(eval, args, 0);
                        }
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                    }
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0) {
                        if (!result.empty()) {
                            context->insertVariable("ans", result[0]);
                            for (size_t k = 0; k < result.size(); ++k) {
                                eval->display(result[k], "ans", false, true);
                            }
                        }
                        break;
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 1 && result.empty()) {
                            stack.push_back(noOutputSentinel());
                        } else {
                            ArrayOf value = k < result.size() ? result[k] : ArrayOf::empty();
                            if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 1
                                && value.name().empty()) {
                                value.name("ans");
                            }
                            stack.push_back(value);
                        }
                    }
                    if (ins.C == 1 && nout == 0) {
                        stack.push_back(noOutputSentinel());
                    }
                } break;
                case OpCode::CALL_CLASSDEF_MEMBER: {
                    if (ins.A >= chunk.constants.size()) {
                        RaiseError(_W(BYTECODE_VM_ERROR_CLASSDEF_MEMBER_CONSTANT_INDEX),
                            L"Nelson:bytecode:classdefMemberConstantIndex");
                    }
                    const std::string& spec = std::get<std::string>(chunk.constants.get(ins.A));
                    std::string className;
                    std::string memberName;
                    if (!splitClassdefMemberSpec(spec, className, memberName)) {
                        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER) + spec,
                            "Nelson:interpreter:undefinedMember");
                    }

                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    const bool explicitCall = (ins.flags & INST_FLAG_INDEX_ONLY) != 0;
                    const uint8_t outputFlags = ins.flags & ~INST_FLAG_INDEX_ONLY;

                    {
                        // Variable fallback. The head of the spec may be a local or context
                        // variable (e.g. a struct captured by an anonymous function); a variable
                        // shadows any class/package of the same name. Covers 'var.member(...)' and
                        // 'var.f1.f2...member(...)', where className carries the middle fields
                        // (spec "s.M.inner" -> className "s.M").
                        const size_t headDot = className.find('.');
                        const std::string headName = headDot == std::string::npos
                            ? className
                            : className.substr(0, headDot);
                        ArrayOf variable;
                        bool hasVariable = tryGetAssignedLocalByName(headName, variable);
                        if (!hasVariable) {
                            hasVariable = context->lookupVariable(headName, variable);
                        }
                        if (hasVariable) {
                            ArrayOf current = applyClassdefMemberMiddlePath(variable, className);
                            ArrayOfVector result;
                            try {
                                result
                                    = evaluateDotReference(current, memberName, args, explicitCall);
                            } catch (const NelsonException&) {
                                if (className == headName + ".Properties") {
                                    raiseTablePropertiesIndexError(
                                        eval, variable, memberName, args);
                                }
                                throw;
                            }
                            for (uint16_t k = 0; k < ins.C; ++k) {
                                stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                            }
                            break;
                        }
                    }

                    std::string effectiveClassName = className;
                    context->resolveImportedClassName(className, effectiveClassName);
                    std::string functionName;

                    bool resolved = false;
                    if (explicitCall) {
                        if (!resolved) {
                            resolved = classdefResolveStaticMethodFunction(effectiveClassName,
                                memberName, functionName, eval->getClassdefAccessContext());
                        }
                    } else if (args.empty()) {
                        if (!resolved) {
                            resolved
                                = classdefResolveConstantPropertyFunction(effectiveClassName,
                                      memberName, functionName, eval->getClassdefAccessContext())
                                || classdefResolveEnumerationMemberFunction(
                                    effectiveClassName, memberName, functionName)
                                || classdefResolveStaticMethodFunction(effectiveClassName,
                                    memberName, functionName, eval->getClassdefAccessContext());
                        }
                    }

                    if (!resolved) {
                        // A package function registered under its full dotted name (e.g.
                        // "Pkg.func" for +Pkg/func.m) is tried before treating the head as a base
                        // function that returns an object. Classdef statics were already resolved
                        // above with their access rules. The lookup uses the path indexer only:
                        // classdef statics live in FunctionsInMemory, so an access-denied static
                        // must stay unreachable. It also covers a package that was not on the path
                        // when this call site was compiled.
                        std::string resolvedImportedClass;
                        if (PathFunctionIndexerManager::getInstance()->functionFileExists(spec)) {
                            functionName = spec;
                        } else if (context->resolveImportedClassName(spec, resolvedImportedClass)) {
                            // The whole spec may name a package-qualified class reachable through
                            // the active imports: after `import pkg.*`, `subpkg.Class(args)`
                            // constructs `pkg.subpkg.Class`. Resolving the full spec keeps the
                            // qualified path instead of calling the head as a base function. The
                            // constructor is registered under the fully-qualified class name.
                            functionName = resolvedImportedClass;
                        } else {
                            // Base function returning an object: the head segment is
                            // called with no argument and the remaining segments are
                            // applied as a member chain (`fn.Inner.Method(args)`,
                            // `py.pkg.Class(args)`), whatever the case of the segments.
                            const size_t headDot = className.find('.');
                            const std::string headName = headDot == std::string::npos
                                ? className
                                : className.substr(0, headDot);
                            FunctionDef* baseDef = nullptr;
                            if (eval->lookupFunction(headName, baseDef)) {
                                ArrayOfVector baseResult = baseDef->evaluateFunction(eval, {}, 1);
                                ArrayOf base
                                    = baseResult.empty() ? ArrayOf::empty() : baseResult[0];
                                base = applyClassdefMemberMiddlePath(base, className);
                                // No explicit-call flag here: `base` is a throwaway object
                                // returned by a same-named base function, not a user instance, so
                                // `ClassName.instanceMethod()` must stay a member lookup that
                                // reports "No such property". Only the variable fallback holds a
                                // real instance and forwards the flag.
                                ArrayOfVector result = evaluateDotReference(base, memberName, args);
                                for (uint16_t k = 0; k < ins.C; ++k) {
                                    stack.push_back(
                                        k < result.size() ? result[k] : ArrayOf::empty());
                                }
                                break;
                            }
                            RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER) + spec,
                                "Nelson:interpreter:undefinedMember");
                        }
                    }

                    FunctionDef* fdef = nullptr;
                    if (!eval->lookupFunction(functionName, fdef) || fdef == nullptr) {
                        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER) + spec,
                            "Nelson:interpreter:undefinedMember");
                    }
                    bool syncCallerContext = needsCallerContextSync(fdef);
                    bool refreshCallerContext = needsCallerContextRefresh(fdef);
                    ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    uint16_t nout = ins.C;
                    ArrayOfVector result;
                    size_t clearEpoch = context->getVariablesClearedEpoch();
                    if ((outputFlags & INST_FLAG_PRINT) != 0 && nout == 1
                        && fdef->outputArgCount() == 0) {
                        result = fdef->evaluateFunction(eval, args, 0);
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                        stack.push_back(noOutputSentinel());
                        break;
                    }
                    try {
                        result = fdef->evaluateFunction(eval, args, static_cast<int>(nout));
                    } catch (const NelsonException& e) {
                        if ((outputFlags & INST_FLAG_PRINT) == 0 || nout != 1
                            || (e.identifier() != L"Nelson:tooManyOutputs"
                                && e.identifier() != L"Nelson:nargoutchk:tooManyOutputs"
                                && e.message()
                                    != Nelson::resolveMessageTemplate(
                                        L"Nelson:interpreter:notEnoughVarargout",
                                        L"Not enough outputs in varargout to satisfy call."))) {
                            throw;
                        }
                        nout = 0;
                        result = fdef->evaluateFunction(eval, args, 0);
                    }
                    refreshAfterVariableClear(clearEpoch);
                    if (refreshCallerContext) {
                        refreshLocalsFromContext();
                    }
                    if ((outputFlags & INST_FLAG_PRINT) != 0 && ins.C == 0) {
                        if (!result.empty()) {
                            context->insertVariable("ans", result[0]);
                            for (size_t k = 0; k < result.size(); ++k) {
                                eval->display(result[k], "ans", false, true);
                            }
                        }
                        break;
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        if ((outputFlags & INST_FLAG_PRINT) != 0 && ins.C == 1 && result.empty()) {
                            stack.push_back(noOutputSentinel());
                        } else {
                            ArrayOf value = k < result.size() ? result[k] : ArrayOf::empty();
                            if ((outputFlags & INST_FLAG_PRINT) != 0 && ins.C == 1
                                && value.name().empty()) {
                                value.name("ans");
                            }
                            stack.push_back(value);
                        }
                    }
                    if (ins.C == 1 && nout == 0) {
                        stack.push_back(noOutputSentinel());
                    }
                } break;
                case OpCode::CALL_NAMED_DYNAMIC: {
                    if (ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:nameIndexOutOfRange",
                            L"BytecodeVM: name index out of range.");
                    }
                    ArrayOf outputSpec = pop();
                    uint16_t nout = outputCountFromSpec(eval, outputSpec);
                    const std::string& name = chunk.names[ins.A];
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    ArrayOf variable;
                    bool hasVariable = tryGetAssignedLocalByNameIndex(ins.A, variable);
                    if (!hasVariable) {
                        hasVariable = context->lookupVariable(name, variable);
                    }
                    if (hasVariable && variable.isFunctionHandle()) {
                        FunctionDef* handleDef = functionDefFromHandle(variable);
                        bool syncCallerContext = needsCallerContextSync(handleDef);
                        bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                        ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                        if (syncCallerContext) {
                            syncAssignedLocalsToContext();
                        }
                        size_t clearEpoch = context->getVariablesClearedEpoch();
                        ArrayOfVector result
                            = callFunctionHandle(eval, variable, args, static_cast<int>(nout));
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                        for (uint16_t k = 0; k < nout; ++k) {
                            stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                        }
                        break;
                    }
                    if (hasVariable && !args.empty()
                        && (variable.isClassType() || variable.isHandle())) {
                        // indexing a class/handle object held in a non-local
                        // variable with a dynamic output count: dispatch to subsref
                        stringVector subtypes;
                        subtypes.push_back("()");
                        ArrayOfVector subsindices;
                        subsindices.push_back(classIndexCell(args));
                        bool haveFunction = false;
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            variable, subtypes, subsindices, haveFunction, static_cast<int>(nout));
                        if (haveFunction) {
                            for (uint16_t k = 0; k < nout; ++k) {
                                stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                            }
                            break;
                        }
                    }
                    FunctionDef* fdef = nullptr;
                    if (!lookupCachedFunction(ins.A, name, fdef)) {
                        Error("Nelson:UndefinedFunction",
                            formatI18nMessage(
                                Nelson::resolveMessageTemplate(
                                    "Nelson:UndefinedFunction:undefinedVariableFunctionValue",
                                    "Undefined variable or function: %s"),
                                name));
                    }
                    bool syncCallerContext = needsCallerContextSync(fdef);
                    bool refreshCallerContext = needsCallerContextRefresh(fdef);
                    ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    size_t clearEpoch = context->getVariablesClearedEpoch();
                    ArrayOfVector result
                        = fdef->evaluateFunction(eval, args, static_cast<int>(nout));
                    refreshAfterVariableClear(clearEpoch);
                    if (refreshCallerContext) {
                        refreshLocalsFromContext();
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                    }
                } break;
                case OpCode::CALL_BUILTIN:
                case OpCode::CALL_MACRO: {
                    FunctionDef* funcDef = std::get<FunctionDef*>(chunk.constants.get(ins.A));
                    if (funcDef == nullptr) {
                        Error(L"Nelson:bytecode:invalidFunctionConstant",
                            L"BytecodeVM: invalid function constant.");
                    }
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    bool syncCallerContext = needsCallerContextSync(funcDef);
                    bool refreshCallerContext = needsCallerContextRefresh(funcDef);
                    ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    size_t clearEpoch = context->getVariablesClearedEpoch();
                    ArrayOfVector result
                        = funcDef->evaluateFunction(eval, args, static_cast<int>(ins.C));
                    refreshAfterVariableClear(clearEpoch);
                    if (refreshCallerContext) {
                        refreshLocalsFromContext();
                    }
                    for (uint16_t k = 0; k < ins.C; ++k) {
                        stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                    }
                } break;
                case OpCode::CALL_HANDLE: {
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    ArrayOf handle = pop();
                    FunctionDef* handleDef = functionDefFromHandle(handle);
                    bool syncCallerContext = needsCallerContextSync(handleDef);
                    bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                    ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    size_t clearEpoch = context->getVariablesClearedEpoch();
                    ArrayOfVector result
                        = callFunctionHandle(eval, handle, args, static_cast<int>(ins.C));
                    refreshAfterVariableClear(clearEpoch);
                    if (refreshCallerContext) {
                        refreshLocalsFromContext();
                    }
                    for (uint16_t k = 0; k < ins.C; ++k) {
                        stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                    }
                } break;
                case OpCode::CALL_HANDLE_DYNAMIC: {
                    ArrayOf outputSpec = pop();
                    uint16_t nout = outputCountFromSpec(eval, outputSpec);
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    ArrayOf handle = pop();
                    FunctionDef* handleDef = functionDefFromHandle(handle);
                    bool syncCallerContext = needsCallerContextSync(handleDef);
                    bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                    ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    size_t clearEpoch = context->getVariablesClearedEpoch();
                    ArrayOfVector result
                        = callFunctionHandle(eval, handle, args, static_cast<int>(nout));
                    refreshAfterVariableClear(clearEpoch);
                    if (refreshCallerContext) {
                        refreshLocalsFromContext();
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        stack.push_back(k < result.size() ? result[k] : ArrayOf::empty());
                    }
                } break;
                case OpCode::EXPAND_COMMA_LIST: {
                    ArrayOf value = pop();
                    ArrayOfVector values = isCommaListSentinel(value) ? commaListValues(value)
                                                                      : ArrayOfVector { value };
                    if (values.size() < ins.A) {
                        Error(L"Nelson:interpreter:insufficientOutputs",
                            L"Insufficient number of outputs from right hand side of equal sign to "
                            L"satisfy assignment.");
                    }
                    for (uint16_t k = 0; k < ins.A; ++k) {
                        stack.push_back(values[k]);
                    }
                } break;
                case OpCode::EXPAND_COMMA_LIST_DYNAMIC: {
                    ArrayOf outputSpec = pop();
                    uint16_t nout = outputCountFromSpec(eval, outputSpec);
                    ArrayOf value = pop();
                    ArrayOfVector values = isCommaListSentinel(value) ? commaListValues(value)
                                                                      : ArrayOfVector { value };
                    if (values.size() < nout) {
                        Error(L"Nelson:interpreter:insufficientOutputs",
                            L"Insufficient number of outputs from right hand side of equal sign to "
                            L"satisfy assignment.");
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        stack.push_back(values[k]);
                    }
                } break;
                case OpCode::SUBIDX_PARENS: {
                    ArrayOfVector indices;
                    if (ins.B == 1) {
                        ArrayOf indexValue = pop();
                        indices.push_back(indexValue);
                        if (isCommaListSentinel(indexValue)) {
                            indices = expandCommaLists(indices);
                        }
                    } else {
                        indices = expandCommaLists(popVector(ins.B));
                    }
                    ArrayOf base = pop();
                    if (frame.chainCollect.active) {
                        frame.chainCollect.subtypes.push_back("()");
                        frame.chainCollect.subsindices.push_back(classIndexCell(indices));
                        stack.push_back(ArrayOf::empty());
                        break;
                    }
                    try {
                        pushParenSubset(base, indices, ins.flags, ins.C);
                    } catch (const NelsonException&) {
                        raiseTablePropertiesIndexError(eval, chunk, frame, pc - 1, indices);
                        throw;
                    }
                } break;
                case OpCode::SUBIDX_BRACES: {
                    ArrayOfVector indices;
                    if (ins.B == 1) {
                        ArrayOf indexValue = pop();
                        indices.push_back(indexValue);
                        if (isCommaListSentinel(indexValue)) {
                            indices = expandCommaLists(indices);
                        }
                    } else {
                        indices = expandCommaLists(popVector(ins.B));
                    }
                    ArrayOf base = pop();
                    if (frame.chainCollect.active) {
                        frame.chainCollect.subtypes.push_back("{}");
                        frame.chainCollect.subsindices.push_back(classIndexCell(indices));
                        stack.push_back(ArrayOf::empty());
                        break;
                    }
                    if (indices.empty()) {
                        Error(L"Nelson:interpreter:indexExpressionExpected",
                            L"Index expression expected.");
                    }
                    if (base.isClassType() || base.isHandle()) {
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back("{}");
                        subsindices.push_back(classIndexCell(indices));
                        bool haveFunction = false;
                        int requestedOutputs
                            = ((ins.flags & INST_FLAG_LIST) != 0 && ins.C != 0) ? ins.C : 1;
                        // comma-separated-list in expression context: the output
                        // count is not fixed by the left-hand side (ins.C <= 1), so
                        // let the class decide via numArgumentsFromSubscript. A count
                        // of 2 or more comes from an explicit multi-target assignment
                        // and is used as-is.
                        if ((ins.flags & INST_FLAG_LIST) != 0 && ins.C <= 1) {
                            int reported = objectNumArgumentsFromSubscript(
                                eval, base, subtypes, subsindices, "Expression");
                            if (reported >= 0) {
                                requestedOutputs = reported;
                            }
                        }
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            base, subtypes, subsindices, haveFunction, requestedOutputs);
                        if (!haveFunction && base.isClassType()) {
                            Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                        }
                        if (!haveFunction) {
                            if (ins.flags & INST_FLAG_LIST) {
                                stack.push_back(indices.size() == 1
                                        ? commaListSentinel(base.vectorContentsAsList(indices[0]))
                                        : commaListSentinel(base.nDimContentsAsList(indices)));
                                break;
                            }
                            ArrayOfVector values = indices.size() == 1
                                ? base.vectorContentsAsList(indices[0])
                                : base.nDimContentsAsList(indices);
                            stack.push_back(values.empty() ? ArrayOf::empty() : values[0]);
                            break;
                        }
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                        } else {
                            stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        }
                        break;
                    }
                    resolveObjectIndices(eval, indices);
                    try {
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(indices.size() == 1
                                    ? commaListSentinel(base.vectorContentsAsList(indices[0]))
                                    : commaListSentinel(base.nDimContentsAsList(indices)));
                            break;
                        }
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0
                            && braceAssignmentParentIsNew(base, indices)) {
                            // Reading a not-yet-existing brace element while building an
                            // assignment chain (w{1}.field with w undefined, c{1}{1} on c = {},
                            // d{3}.k on d = {1}) yields an empty value. The next field or index
                            // assignment then creates the nested structure and grows the cell on
                            // store-back.
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                        ArrayOf directValue;
                        if (tryReadCellScalarContentDirect(base, indices, directValue)) {
                            stack.push_back(directValue);
                            break;
                        }
                        ArrayOfVector values = indices.size() == 1
                            ? base.vectorContentsAsList(indices[0])
                            : base.nDimContentsAsList(indices);
                        if (values.empty()) {
                            raiseTablePropertiesIndexError(eval, chunk, frame, pc - 1, indices);
                            if ((ins.flags & (INST_FLAG_PRINT | INST_FLAG_QUIET)) != 0) {
                                stack.push_back(noOutputSentinel());
                                break;
                            }
                            Error(L"Nelson:interpreter:emptyExpression", L"Empty expression!");
                        }
                        stack.push_back(values[0]);
                    } catch (const NelsonException&) {
                        raiseTablePropertiesIndexError(eval, chunk, frame, pc - 1, indices);
                        throw;
                    }
                } break;
                case OpCode::CHAIN_BEGIN: {
                    // Activate whole-chain collection only when the head is a
                    // Redefines* object AND the FIRST operation is the kind that
                    // class redefines (ins.B: 0="()", 1="{}", 2="."). A non-
                    // redefined leading op (e.g. obj.Data{i} on a RedefinesBrace,
                    // where .Data is ordinary property access) stays per-level.
                    frame.chainCollect.active = false;
                    if (ins.A < frame.locals.size() && isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        const ArrayOf& head = frame.locals[ins.A];
                        if (head.isClassType() && classdefLoadClass(head.classType())) {
                            const std::string& cls = head.classType();
                            const bool matches
                                = (ins.B == 0
                                      && classdefIsA(cls, NLS_MIXIN_INDEXING_REDEFINES_PAREN))
                                || (ins.B == 1
                                    && classdefIsA(cls, NLS_MIXIN_INDEXING_REDEFINES_BRACE))
                                || (ins.B == 2
                                    && classdefIsA(cls, NLS_MIXIN_INDEXING_REDEFINES_DOT));
                            if (matches) {
                                frame.chainCollect.active = true;
                                frame.chainCollect.base = head;
                                frame.chainCollect.subtypes.clear();
                                frame.chainCollect.subsindices.clear();
                            }
                        }
                    }
                } break;
                case OpCode::CHAIN_END: {
                    if (frame.chainCollect.active) {
                        pop(); // discard the final chain sentinel
                        ArrayOf base = frame.chainCollect.base;
                        stringVector subtypes = frame.chainCollect.subtypes;
                        ArrayOfVector subsindices = frame.chainCollect.subsindices;
                        frame.chainCollect.active = false;
                        ArrayOf wholeChainResult;
                        if (dispatchRedefinesWholeChain(
                                eval, base, subtypes, subsindices, wholeChainResult)) {
                            stack.push_back(wholeChainResult);
                            break;
                        }
                        bool haveFunction = false;
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            base, subtypes, subsindices, haveFunction, 1);
                        if (!haveFunction) {
                            // Fallback: apply per level (values correct).
                            ArrayOf current = base;
                            for (size_t i = 0; i < subtypes.size(); ++i) {
                                stringVector oneType { subtypes[i] };
                                ArrayOfVector oneIdx { subsindices[i] };
                                bool stepHasFunction = false;
                                ArrayOfVector stepResult = eval->bytecodeExtractClass(
                                    current, oneType, oneIdx, stepHasFunction, 1);
                                if (stepHasFunction) {
                                    current = stepResult.empty() ? ArrayOf::empty() : stepResult[0];
                                } else if (subtypes[i] == ".") {
                                    current = readDotValueGeneric(
                                        eval, current, subsindices[i].asUtf8String());
                                } else {
                                    ArrayOfVector idx;
                                    if (subsindices[i].isCell()) {
                                        const auto* els = static_cast<const ArrayOf*>(
                                            subsindices[i].dataPointer());
                                        indexType n = subsindices[i].elementCount();
                                        for (indexType k = 0; els != nullptr && k < n; ++k) {
                                            idx.push_back(els[k]);
                                        }
                                    }
                                    if (subtypes[i] == "{}") {
                                        ArrayOfVector c = idx.size() == 1
                                            ? current.vectorContentsAsList(idx[0])
                                            : current.nDimContentsAsList(idx);
                                        current = c.empty() ? ArrayOf::empty() : c[0];
                                    } else {
                                        current = idx.size() == 1 ? current.vectorSubset(idx[0])
                                                                  : current.nDimSubset(idx);
                                    }
                                }
                            }
                            result.clear();
                            result.push_back(current);
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                    }
                } break;
                case OpCode::SUBIDX_DOT: {
                    if (frame.chainCollect.active && ins.A == 0 && ins.C == 0) {
                        pop(); // discard chain sentinel
                        const auto& collectField
                            = std::get<std::string>(chunk.constants.get(ins.B));
                        frame.chainCollect.subtypes.push_back(".");
                        frame.chainCollect.subsindices.push_back(
                            ArrayOf::characterArray(collectField));
                        stack.push_back(ArrayOf::empty());
                        break;
                    }
                    ArrayOfVector params = expandCommaLists(popVector(ins.A));
                    ArrayOf base = pop();
                    const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                    ArrayOf tableVariable;
                    if (params.empty() && ins.C == 0
                        && tryFastTableDotVariable(eval, base, field, tableVariable)) {
                        stack.push_back(tableVariable);
                        break;
                    }
                    if (ins.C != 0 && (ins.flags & INST_FLAG_LIST) == 0
                        && tryFastTableDotVariableIndex(eval, base, field, params, tableVariable)) {
                        stack.push_back(tableVariable);
                        break;
                    }
                    if (base.isClassType()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            stringVector fields = base.fieldNames();
                            if (std::find(fields.begin(), fields.end(), field) != fields.end()) {
                                ArrayOfVector raw = getClassdefFieldAsList(eval, base, field);
                                stack.push_back(raw.empty() ? ArrayOf::empty() : raw[0]);
                                break;
                            }
                        }
                        if (base.dataClass() == NLS_CLASS_ARRAY
                            && eval->getClassdefAccessContext() == base.classType()) {
                            // Inside its own method, a classic (struct-based) object reads its
                            // data field directly. obj.field(args) then indexes the field's value
                            // instead of going through subsref.
                            stringVector classFields = base.fieldNames();
                            if (std::find(classFields.begin(), classFields.end(), field)
                                != classFields.end()) {
                                ArrayOfVector fv = base.fieldAsList(field);
                                ArrayOf fieldValue = fv.empty() ? ArrayOf::empty() : fv[0];
                                if (ins.C == 0 || params.empty()) {
                                    stack.push_back(fieldValue);
                                } else {
                                    stack.push_back(
                                        subsetClassicFieldValue(eval, fieldValue, params));
                                }
                                break;
                            }
                        }
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArray(field));
                        if (ins.C != 0) {
                            subtypes.push_back("()");
                            subsindices.push_back(classIndexCell(params));
                        }
                        bool haveFunction = false;
                        // A bare statement (INST_FLAG_QUIET) requests no output: `obj.m(args);`
                        // whose body never assigns its declared varargout must run like `m(obj)`,
                        // not with nargout 1. ins.C also means parens-present and is never 0 at
                        // compile time, so the intent arrives as QUIET.
                        int requestedOutputs = (ins.flags & INST_FLAG_QUIET) != 0
                            ? 0
                            : (((ins.flags & INST_FLAG_LIST) != 0 && ins.C != 0) ? ins.C : 1);
                        if (ins.C != 0) {
                            ArrayOf propertyValue;
                            if (tryReadClassdefPlainProperty(eval, base, field, propertyValue)
                                && propertyValue.isFunctionHandle()) {
                                ArrayOfVector result = invokeFunctionHandleValue(
                                    propertyValue, params, requestedOutputs);
                                if (ins.flags & INST_FLAG_LIST) {
                                    stack.push_back(commaListSentinel(result));
                                    break;
                                }
                                stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                                break;
                            }
                        }
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            base, subtypes, subsindices, haveFunction, requestedOutputs);
                        if (!haveFunction) {
                            if (ins.C != 0) {
                                ArrayOfVector methodResult;
                                // A property accessed with arguments (obj.prop(args)) is a field
                                // access followed by an index/call, not a method call, even if a
                                // method differing only in case exists. The match is
                                // case-sensitive: obj.name(args) still calls method 'name' when
                                // only property 'Name' exists.
                                ClassdefPropertyDefinition propertyForCall;
                                const bool isProperty = base.isScalar() && base.isClassType()
                                    && classdefLoadClass(base.classType())
                                    && classdefPropertyDefinition(
                                        base.classType(), field, propertyForCall)
                                    && propertyForCall.name == field;
                                if (!isProperty
                                    && eval->bytecodeInvokeObjectMethodIfExists(
                                        base, field, params, requestedOutputs, methodResult)) {
                                    result = methodResult;
                                } else {
                                    ArrayOfVector fieldValues
                                        = getClassdefFieldAsList(eval, base, field);
                                    result = indexFirstValue(fieldValues, params);
                                }
                            } else {
                                // a no-argument method accessed without parentheses
                                // (obj.method) on an instance is invoked, not read
                                // as a property.
                                ArrayOfVector methodResult;
                                if (base.isScalar() && base.isClassType()
                                    && classdefLoadClass(base.classType())
                                    && !classdefHasProperty(base.classType(), field)
                                    && eval->bytecodeInvokeObjectMethodIfExists(
                                        base, field, {}, 1, methodResult)) {
                                    result = methodResult;
                                } else {
                                    result = getClassdefFieldAsList(eval, base, field);
                                }
                            }
                        }
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                            break;
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (base.isHandle() || base.isGraphicsObject()) {
                        int requestedOutputs = (ins.flags & INST_FLAG_QUIET) != 0
                            ? 0
                            : ((ins.flags & INST_FLAG_LIST) != 0 ? static_cast<int>(ins.C) : 1);
                        ArrayOfVector result;
                        bool methodInvoked = ins.C != 0
                            && eval->bytecodeInvokeObjectMethodIfExists(
                                base, field, params, requestedOutputs, result);
                        if (!methodInvoked) {
                            result = getOrInvokeHandle(eval, base, field, params);
                        }
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                            break;
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (!base.isStruct()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            checkDotAssignParent(base);
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                        Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                    }
                    if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                        stringVector fields = base.fieldNames();
                        if (std::find(fields.begin(), fields.end(), field) == fields.end()) {
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                    }
                    if (ins.C == 0 && (ins.flags & INST_FLAG_LIST) == 0 && base.isScalar()) {
                        stack.push_back(base.field(field));
                        break;
                    }
                    if (ins.C == 0 && (ins.flags & INST_FLAG_LIST) != 0) {
                        ArrayOf directRow;
                        if (tryStructFieldScalarDoubleRowSentinel(base, field, directRow)) {
                            stack.push_back(directRow);
                            break;
                        }
                    }
                    ArrayOfVector fieldValues = base.fieldAsList(field);
                    ArrayOf fieldValue = fieldValues.empty() ? ArrayOf::empty() : fieldValues[0];
                    if (ins.C != 0) {
                        if (fieldValue.isFunctionHandle()) {
                            int requestedOutputs = (ins.flags & INST_FLAG_QUIET) != 0
                                ? 0
                                : ((ins.flags & INST_FLAG_LIST) != 0 ? static_cast<int>(ins.C) : 1);
                            FunctionDef* handleDef = functionDefFromHandle(fieldValue);
                            bool syncCallerContext = needsCallerContextSync(handleDef);
                            bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                            ensureContextScopeForCall(syncCallerContext, refreshCallerContext);
                            if (syncCallerContext) {
                                syncAssignedLocalsToContext();
                            }
                            size_t clearEpoch = context->getVariablesClearedEpoch();
                            ArrayOfVector result
                                = callFunctionHandle(eval, fieldValue, params, requestedOutputs);
                            refreshAfterVariableClear(clearEpoch);
                            if (refreshCallerContext) {
                                refreshLocalsFromContext();
                            }
                            if (ins.flags & INST_FLAG_LIST) {
                                stack.push_back(commaListSentinel(result));
                                break;
                            }
                            if (requestedOutputs == 0) {
                                stack.push_back(noOutputSentinel());
                                break;
                            }
                            stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                            break;
                        }
                        if (params.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        ArrayOfVector result;
                        try {
                            result = indexFirstValue(fieldValues, params);
                        } catch (const NelsonException&) {
                            raiseTablePropertiesIndexError(eval, chunk, frame, pc - 1, params);
                            throw;
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (ins.flags & INST_FLAG_LIST) {
                        if (fieldValues.empty()) {
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                        stack.push_back(commaListSentinel(fieldValues));
                        break;
                    }
                    stack.push_back(fieldValue);
                } break;
                case OpCode::SUBIDX_DOTDYN: {
                    ArrayOf fieldValue = pop();
                    ArrayOf base = pop();
                    if (isIndexingOperationValue(fieldValue)) {
                        ArrayOf applied = applyIndexingOperationRead(eval, base, fieldValue);
                        if (ins.flags & INST_FLAG_LIST) {
                            ArrayOfVector single;
                            single.push_back(applied);
                            stack.push_back(commaListSentinel(single));
                            break;
                        }
                        stack.push_back(applied);
                        break;
                    }
                    ArrayOfVector classResult;
                    if (tryExtractClassDynamicField(eval, base, fieldValue, 1, classResult)) {
                        stack.push_back(classResult.empty() ? ArrayOf::empty() : classResult[0]);
                        break;
                    }
                    std::string field;
                    try {
                        field = fieldValue.asUtf8String();
                    } catch (const NelsonException&) {
                        Error(L"Nelson:interpreter:invalidDynamicFieldName",
                            L"Dynamic field name must be a string.");
                    }
                    if (base.isHandle() || base.isGraphicsObject()) {
                        ArrayOfVector params;
                        ArrayOfVector result = getOrInvokeHandle(eval, base, field, params);
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                            break;
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (base.isClassType()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            stringVector fields = base.fieldNames();
                            if (std::find(fields.begin(), fields.end(), field) != fields.end()) {
                                ArrayOfVector raw = getClassdefFieldAsList(eval, base, field);
                                stack.push_back(raw.empty() ? ArrayOf::empty() : raw[0]);
                                break;
                            }
                        }
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArray(field));
                        bool haveFunction = false;
                        int requestedOutputs
                            = ((ins.flags & INST_FLAG_LIST) != 0 && ins.C != 0) ? ins.C : 1;
                        ArrayOfVector result = eval->bytecodeExtractClass(
                            base, subtypes, subsindices, haveFunction, requestedOutputs);
                        if (!haveFunction) {
                            result = getClassdefFieldAsList(eval, base, field);
                        }
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                            break;
                        }
                        stack.push_back(result.empty() ? ArrayOf::empty() : result[0]);
                        break;
                    }
                    if (!base.isStruct()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            checkDotAssignParent(base);
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                        Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                    }
                    if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                        stringVector fields = base.fieldNames();
                        if (std::find(fields.begin(), fields.end(), field) == fields.end()) {
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                    }
                    if (ins.flags & INST_FLAG_LIST) {
                        ArrayOf directRow;
                        if (tryStructFieldScalarDoubleRowSentinel(base, field, directRow)) {
                            stack.push_back(directRow);
                            break;
                        }
                    }
                    ArrayOfVector fieldValues = base.fieldAsList(field);
                    if (ins.flags & INST_FLAG_LIST) {
                        if (fieldValues.empty()) {
                            stack.push_back(ArrayOf::empty());
                            break;
                        }
                        stack.push_back(commaListSentinel(fieldValues));
                        break;
                    }
                    stack.push_back(fieldValues.empty() ? ArrayOf::empty() : fieldValues[0]);
                } break;
                case OpCode::ASGN_STRING_SCALAR_CONST: {
                    const ConstantValue& constant = chunk.constants.get(ins.C);
                    const auto* element = std::get_if<ArrayOf>(&constant);
                    if (element == nullptr) {
                        Error(L"Nelson:bytecode:invalidConstant",
                            L"Bytecode constant is not an ArrayOf value.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    auto operation
                        = [&]() { assignConstantStringScalarIndex(eval, frame, ins, *element); };
                    if (Profiler::getInstance()->isOn()) {
                        profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, operation);
                    } else {
                        operation();
                    }
                } break;
                case OpCode::ASGN_PARENS_CONST: {
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->isLockedVariable(chunk.localNames[ins.A])) {
                        Error(L"Nelson:interpreter:redefinePermanent",
                            L"Redefining permanent variable.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf index = constantAsArray(chunk.constants.get(ins.B));
                        ArrayOf value = constantAsArray(chunk.constants.get(ins.C));
                        assignConstantParensIndex(eval, frame, ins.A, index, value);
                    });
                } break;
                case OpCode::ASGN_PARENS: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->isLockedVariable(chunk.localNames[ins.A])) {
                        Error(L"Nelson:interpreter:redefinePermanent",
                            L"Redefining permanent variable.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices;
                        ArrayOf value;
                        if (ins.B == 2) {
                            ArrayOf colIndex = pop();
                            ArrayOf rowIndex = pop();
                            value = pop();
                            ArrayOf& base = frame.locals[ins.A];
                            if (tryAssignDoubleScalar2DDirect(base, rowIndex, colIndex, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            if (tryAssignDoubleMatrix2DDirect(base, rowIndex, colIndex, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            if (tryAssignStringScalar2DDirect(base, rowIndex, colIndex, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            indices.reserve(2);
                            indices.push_back(rowIndex);
                            indices.push_back(colIndex);
                            indices = expandCommaLists(indices);
                        } else if (ins.B == 1) {
                            ArrayOf index = pop();
                            value = pop();
                            ArrayOf& base = frame.locals[ins.A];
                            if (tryAssignDoubleScalarLinearDirect(base, index, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            if (tryAssignDoubleVectorLinearDirect(base, index, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            if (tryAssignDoubleLogicalMaskDirect(base, index, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            if (isColonIndex(index)) {
                                if (tryAssignStringScalarAllElements(base, value)
                                    || tryAssignCellColonFromScalarCell(base, value)) {
                                    markRuntimeAssigned(frame, ins.A);
                                    return;
                                }
                            }
                            if (tryAssignStringScalarLinearDirect(base, index, value)) {
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            indices.reserve(1);
                            indices.push_back(index);
                            indices = expandCommaLists(indices);
                        } else {
                            indices = expandCommaLists(popVector(ins.B));
                            value = pop();
                        }
                        if (indices.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        ArrayOf& base = frame.locals[ins.A];
                        if (tryAssignStringScalarNDimDirect(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (tryAssignDoubleScalarNDimDirect(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isClassType() || base.isHandle()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("()");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                if (base.isHandle()) {
                                    normalizeParensAssignmentValueForColon(indices, value);
                                    if (tryAssignVectorColon(base, indices, value)) {
                                        markRuntimeAssigned(frame, ins.A);
                                        return;
                                    }
                                    if (tryAssignEmptyCellColumn(base, indices, value)) {
                                        markRuntimeAssigned(frame, ins.A);
                                        return;
                                    }
                                    expandColonParensAssignmentIndices(base, indices, value);
                                    normalizeParensAssignmentValueForTarget(indices, value);
                                    expandClassdefAssignmentBaseWithDefaults(
                                        eval, base, indices, value);
                                    if (indices.size() == 1) {
                                        base.setVectorSubset(indices[0], value);
                                    } else if (!tryAssignRowColonSlice(base, indices, value)) {
                                        base.setNDimSubset(indices, value);
                                    }
                                    markRuntimeAssigned(frame, ins.A);
                                    return;
                                }
                                if (classdefLoadClass(base.classType())) {
                                    normalizeParensAssignmentValueForColon(indices, value);
                                    if (tryAssignVectorColon(base, indices, value)) {
                                        markRuntimeAssigned(frame, ins.A);
                                        return;
                                    }
                                    if (tryAssignEmptyCellColumn(base, indices, value)) {
                                        markRuntimeAssigned(frame, ins.A);
                                        return;
                                    }
                                    expandColonParensAssignmentIndices(base, indices, value);
                                    normalizeParensAssignmentValueForTarget(indices, value);
                                    expandClassdefAssignmentBaseWithDefaults(
                                        eval, base, indices, value);
                                    if (indices.size() == 1) {
                                        base.setVectorSubset(indices[0], value);
                                    } else if (!tryAssignRowColonSlice(base, indices, value)) {
                                        base.setNDimSubset(indices, value);
                                    }
                                    markRuntimeAssigned(frame, ins.A);
                                    return;
                                }
                                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        resolveObjectIndices(eval, indices);
                        normalizeParensAssignmentValueForColon(indices, value);
                        if (tryAssignDoubleScalar2D(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (tryAssignVectorColon(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (tryAssignEmptyCellColumn(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        expandColonParensAssignmentIndices(base, indices, value);
                        normalizeParensAssignmentValueForTarget(indices, value);
                        expandClassdefAssignmentBaseWithDefaults(eval, base, indices, value);
                        if (indices.size() == 1) {
                            base.setVectorSubset(indices[0], value);
                        } else if (!tryAssignRowColonSlice(base, indices, value)) {
                            base.setNDimSubset(indices, value);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_BRACES: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->isLockedVariable(chunk.localNames[ins.A])) {
                        Error(L"Nelson:interpreter:redefinePermanent",
                            L"Redefining permanent variable.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices;
                        if (ins.B == 1) {
                            ArrayOf indexValue = pop();
                            indices.push_back(indexValue);
                            if (isCommaListSentinel(indexValue)) {
                                indices = expandCommaLists(indices);
                            }
                        } else {
                            indices = expandCommaLists(popVector(ins.B));
                        }
                        ArrayOf value = pop();
                        if (indices.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        ArrayOf& base = frame.locals[ins.A];
                        if (base.isClassType() || base.isHandle()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("{}");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction && base.isClassType()) {
                                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                            }
                            if (!haveFunction) {
                                checkBracesAssignTarget(base);
                                expandColonContentAssignmentIndices(base, indices);
                                try {
                                    if (indices.size() == 1) {
                                        base.setVectorContentsAsList(indices[0], values);
                                    } else {
                                        base.setNDimContentsAsList(indices, values);
                                    }
                                } catch (const NelsonException& e) {
                                    if (e.message()
                                            == Nelson::resolveMessageTemplate(
                                                L"Nelson:interpreter:illegalZeroNegativeIndex",
                                                L"Illegal zero or negative index")
                                        || e.message()
                                            == resolveMessageTemplate(
                                                L"Nelson:interpreter:cannotConvertStringData",
                                                L"Cannot convert string data types to indices.")) {
                                        Error(L"Nelson:interpreter:notEnoughRhsValues",
                                            L"Not enough right hand side values to satisfy left "
                                            L"hand side expression.");
                                    }
                                    throw;
                                }
                                markRuntimeAssigned(frame, ins.A);
                                return;
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        checkBracesAssignTarget(base);
                        resolveObjectIndices(eval, indices);
                        if (tryAssignCellColonFromScalarCell(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (tryAssignCellScalarContentDirect(base, indices, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        expandColonContentAssignmentIndices(base, indices);
                        if ((base.isCell() || base.isEmpty()) && indices.size() == 1
                            && indices[0].elementCount() == 1
                            && !indices[0].isRowVectorCharacterArray()) {
                            base.setVectorContents(indices[0], value);
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        try {
                            if (indices.size() == 1) {
                                base.setVectorContentsAsList(indices[0], values);
                            } else {
                                base.setNDimContentsAsList(indices, values);
                            }
                        } catch (const NelsonException& e) {
                            if (e.message()
                                    == Nelson::resolveMessageTemplate(
                                        L"Nelson:interpreter:illegalZeroNegativeIndex",
                                        L"Illegal zero or negative index")
                                || e.message()
                                    == resolveMessageTemplate(
                                        L"Nelson:interpreter:cannotConvertStringData",
                                        L"Cannot convert string data types to indices.")) {
                                Error(L"Nelson:interpreter:notEnoughRhsValues",
                                    L"Not enough right hand side values to satisfy left hand side "
                                    L"expression.");
                            }
                            throw;
                        }
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_BRACES_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    auto operation = [&]() {
                        ArrayOf& base = frame.locals[ins.A];
                        const ArrayOf& index = frame.locals[ins.B];
                        const ArrayOf& value = frame.locals[ins.C];
                        if (tryAssignCellScalarContentDirect(base, index, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        ArrayOfVector indices;
                        indices.push_back(index);
                        if (base.isCell() || base.isEmpty()) {
                            resolveObjectIndices(eval, indices);
                        }
                        if ((base.isCell() || base.isEmpty()) && indices[0].elementCount() == 1
                            && !indices[0].isRowVectorCharacterArray()) {
                            ArrayOf mutableIndex = indices[0];
                            ArrayOf mutableValue = value;
                            base.setVectorContents(mutableIndex, mutableValue);
                        } else {
                            base = assignBracesToValue(eval, base, indices, value);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    };
                    if (Profiler::getInstance()->isOn()) {
                        profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, operation);
                    } else {
                        operation();
                    }
                } break;
                case OpCode::ASGN_BRACES_LOCAL_COL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= frame.locals.size() || ins.flags == 0) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    auto operation = [&]() {
                        ArrayOf& base = frame.locals[ins.A];
                        const ArrayOf& rowIndex = frame.locals[ins.B];
                        const ArrayOf& value = frame.locals[ins.C];
                        indexType row = 0;
                        indexType column = static_cast<indexType>(ins.flags);
                        if (base.isCell() && !base.isSparse()
                            && scalarDoubleOrdinalIndex(rowIndex, row)
                            && row <= base.dimensionLength(0)
                            && column <= base.dimensionLength(1)) {
                            indexType rows = base.dimensionLength(0);
                            auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
                            elements[(column - 1) * rows + row - 1] = value;
                        } else {
                            ArrayOfVector indices;
                            indices.push_back(rowIndex);
                            indices.push_back(ArrayOf::doubleScalar(static_cast<double>(column)));
                            base = assignBracesToValue(eval, base, indices, value);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    };
                    if (Profiler::getInstance()->isOn()) {
                        profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, operation);
                    } else {
                        operation();
                    }
                } break;
                case OpCode::ASGN_BRACES_CONST_COL: {
                    if (ins.A >= frame.locals.size() || ins.B >= frame.locals.size()
                        || ins.C >= chunk.constants.size() || ins.flags == 0) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    auto operation = [&]() {
                        ArrayOf& base = frame.locals[ins.A];
                        const ArrayOf& rowIndex = frame.locals[ins.B];
                        ArrayOf value = constantAsArray(chunk.constants.get(ins.C));
                        indexType row = 0;
                        indexType column = static_cast<indexType>(ins.flags);
                        if (base.isCell() && !base.isSparse()
                            && scalarDoubleOrdinalIndex(rowIndex, row)
                            && row <= base.dimensionLength(0)
                            && column <= base.dimensionLength(1)) {
                            indexType rows = base.dimensionLength(0);
                            auto* elements = static_cast<ArrayOf*>(base.readWriteDataPointer());
                            elements[(column - 1) * rows + row - 1] = value;
                        } else {
                            ArrayOfVector indices;
                            indices.push_back(rowIndex);
                            indices.push_back(ArrayOf::doubleScalar(static_cast<double>(column)));
                            base = assignBracesToValue(eval, base, indices, value);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    };
                    if (Profiler::getInstance()->isOn()) {
                        profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, operation);
                    } else {
                        operation();
                    }
                } break;
                case OpCode::ASGN_DOT_PLUS_CONST: {
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.constants.size()
                        || ins.C >= chunk.constants.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                        double increment
                            = constantAsArray(chunk.constants.get(ins.C)).asDoubleScalar();
                        ArrayOf& base = frame.locals[ins.A];
                        if (tryIncrementStructScalarDoubleFieldDirect(base, field, increment)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        ArrayOf value = binaryOperator(eval, OpCode::OP_PLUS,
                            readDotValueGeneric(eval, base, field),
                            ArrayOf::doubleScalar(increment));
                        assignDotValueGeneric(eval, base, field, value);
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOT_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.constants.size()
                        || ins.C >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                        ArrayOf value;
                        if (isRuntimeAssignedLocalSlot(frame, ins.C)) {
                            value = frame.locals[ins.C];
                        } else if (ins.C < chunk.localNames.size()
                            && !chunk.localNames[ins.C].empty()) {
                            if (!context->lookupVariable(chunk.localNames[ins.C], value)) {
                                raiseUndefinedVariable(context, chunk, chunk.localNames[ins.C]);
                            }
                        } else {
                            Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                L"Undefined variable.");
                        }
                        ArrayOf& base = frame.locals[ins.A];
                        if (!tryAssignStructScalarFieldDirect(base, field, value)) {
                            assignDotValueGeneric(eval, base, field, value);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOT_DOT_PLUS_SMALL: {
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.constants.size()
                        || ins.C >= chunk.constants.size() || ins.flags == 0) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        const auto& parentField = std::get<std::string>(chunk.constants.get(ins.B));
                        const auto& childField = std::get<std::string>(chunk.constants.get(ins.C));
                        double increment = static_cast<double>(ins.flags);
                        ArrayOf& base = frame.locals[ins.A];
                        if (tryIncrementNestedStructScalarDoubleFieldDirect(
                                base, parentField, childField, increment)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        ArrayOf parent = readDotValueGeneric(eval, base, parentField);
                        ArrayOf value = binaryOperator(eval, OpCode::OP_PLUS,
                            readDotValueGeneric(eval, parent, childField),
                            ArrayOf::doubleScalar(increment));
                        assignDotValueGeneric(eval, parent, childField, value);
                        assignDotValueGeneric(eval, base, parentField, parent);
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOT_DOT_LOCAL: {
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.constants.size()
                        || ins.C >= chunk.constants.size() || ins.flags == 0) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    uint8_t valueSlot = static_cast<uint8_t>(ins.flags - 1);
                    if (valueSlot >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        const auto& parentField = std::get<std::string>(chunk.constants.get(ins.B));
                        const auto& childField = std::get<std::string>(chunk.constants.get(ins.C));
                        ArrayOf value;
                        if (isRuntimeAssignedLocalSlot(frame, valueSlot)) {
                            value = frame.locals[valueSlot];
                        } else if (valueSlot < chunk.localNames.size()
                            && !chunk.localNames[valueSlot].empty()) {
                            if (!context->lookupVariable(chunk.localNames[valueSlot], value)) {
                                raiseUndefinedVariable(context, chunk, chunk.localNames[valueSlot]);
                            }
                        } else {
                            Error(L"Nelson:UndefinedVariable:undefinedVariable",
                                L"Undefined variable.");
                        }
                        ArrayOf& base = frame.locals[ins.A];
                        if (!tryAssignNestedStructScalarFieldDirect(
                                base, parentField, childField, value)) {
                            ArrayOf parent = readDotValueForAssign(eval, base, parentField);
                            assignDotValueGeneric(eval, parent, childField, value);
                            assignDotValueGeneric(eval, base, parentField, parent);
                        }
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOT: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf value = pop();
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                        // Mutate the slot in place (owners == 1), as ASGN_DOT_VALUE does, without
                        // the base copy of the LOAD_LOCAL + *_VALUE + ASSIGN sequence. That copy
                        // made building a struct one field at a time O(nFields^2).
                        ArrayOf& base = frame.locals[ins.A];
                        ArrayOfVector values;
                        values.push_back(value);
                        if (tryFastTableDotAssign(eval, base, field, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArray(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(L"Nelson:interpreter:invalidFieldAssignment",
                                    L"Cannot apply A.field_name = B to non struct-array object A.");
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        checkDotAssignTarget(base);
                        base.setFieldAsList(field, values);
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOTDYN: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf fieldValue = pop();
                        ArrayOf value = pop();
                        ArrayOf& base = frame.locals[ins.A];
                        if (isIndexingOperationValue(fieldValue)) {
                            base = applyIndexingOperationWrite(eval, base, fieldValue, 0, value);
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (tryAssignClassDynamicField(eval, base, fieldValue, value)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        std::string field;
                        try {
                            field = fieldValue.asUtf8String();
                        } catch (const NelsonException&) {
                            Error(L"Nelson:interpreter:invalidDynamicFieldName",
                                L"Dynamic field name must be a string.");
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArray(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(L"Nelson:interpreter:invalidFieldAssignment",
                                    L"Cannot apply A.field_name = B to non struct-array object A.");
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        checkDotAssignTarget(base);
                        base.setFieldAsList(field, values);
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_PARENS_DOT: {
                    if (ins.A >= frame.locals.size()) {
                        Error(L"Nelson:bytecode:localSlotOutOfRange",
                            L"BytecodeVM: local slot out of range.");
                    }
                    seedAssignBaseFromContext(ins.A);
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        if (ins.C >= chunk.constants.size()) {
                            Error(L"Nelson:bytecode:fieldConstantIndexOutOfRange",
                                L"BytecodeVM: field constant index out of range.");
                        }
                        ArrayOfVector indices;
                        if (ins.B == 1) {
                            ArrayOf indexValue = pop();
                            indices.push_back(indexValue);
                            if (isCommaListSentinel(indexValue)) {
                                indices = expandCommaLists(indices);
                            }
                        } else {
                            indices = expandCommaLists(popVector(ins.B));
                        }
                        ArrayOf value = pop();
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.C));
                        ArrayOf& base = frame.locals[ins.A];
                        if (!isRuntimeAssignedLocalSlot(frame, ins.A)) {
                            base = ArrayOf::empty();
                        }
                        if (base.isEmpty() && base.isDoubleClass() && !base.isSparse()) {
                            // an indexed field assignment into an undefined or []
                            // base creates a struct array
                            stringVector structFields;
                            structFields.push_back(field);
                            base.promoteType(NLS_STRUCT_ARRAY, structFields);
                        }
                        if (!base.isClassType() && !base.isHandle()) {
                            resolveObjectIndices(eval, indices);
                        }
                        if (tryAssignStructFieldIndexedDirect(base, field, indices, value, false)) {
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (!base.isClassType() && !base.isHandle() && !base.isGraphicsObject()) {
                            checkDotAssignTarget(base);
                        }
                        ArrayOf workingBase = base;
                        ArrayOf parentBase = indices.size() == 1
                            ? workingBase.vectorSubset(indices[0])
                            : workingBase.nDimSubset(indices);
                        ArrayOf assignedParent = assignDotToValue(eval, parentBase, field, value);
                        workingBase
                            = assignParensToValue(eval, workingBase, indices, assignedParent);
                        base = workingBase;
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_PARENS_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        if (indices.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        if (base.isClassType() || base.isHandle()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("()");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                if (base.isHandle()) {
                                    alignStructFieldsForSubsetAssign(base, value);
                                    normalizeParensAssignmentValueForColon(indices, value);
                                    if (tryAssignVectorColon(base, indices, value)) {
                                        stack.push_back(base);
                                        return;
                                    }
                                    if (tryAssignEmptyCellColumn(base, indices, value)) {
                                        stack.push_back(base);
                                        return;
                                    }
                                    expandColonParensAssignmentIndices(base, indices, value);
                                    normalizeParensAssignmentValueForTarget(indices, value);
                                    expandClassdefAssignmentBaseWithDefaults(
                                        eval, base, indices, value);
                                    if (indices.size() == 1) {
                                        if (!assignStructVectorSubset(base, indices[0], value)) {
                                            base.setVectorSubset(indices[0], value);
                                        }
                                    } else if (tryAssignRowColonSlice(base, indices, value)) {
                                        stack.push_back(base);
                                        return;
                                    } else {
                                        base.setNDimSubset(indices, value);
                                    }
                                    stack.push_back(base);
                                    return;
                                }
                                if (classdefLoadClass(base.classType())) {
                                    alignStructFieldsForSubsetAssign(base, value);
                                    normalizeParensAssignmentValueForColon(indices, value);
                                    if (tryAssignVectorColon(base, indices, value)) {
                                        stack.push_back(base);
                                        return;
                                    }
                                    if (tryAssignEmptyCellColumn(base, indices, value)) {
                                        stack.push_back(base);
                                        return;
                                    }
                                    expandColonParensAssignmentIndices(base, indices, value);
                                    normalizeParensAssignmentValueForTarget(indices, value);
                                    expandClassdefAssignmentBaseWithDefaults(
                                        eval, base, indices, value);
                                    if (indices.size() == 1) {
                                        if (!assignStructVectorSubset(base, indices[0], value)) {
                                            base.setVectorSubset(indices[0], value);
                                        }
                                    } else if (tryAssignRowColonSlice(base, indices, value)) {
                                        stack.push_back(base);
                                        return;
                                    } else {
                                        base.setNDimSubset(indices, value);
                                    }
                                    stack.push_back(base);
                                    return;
                                }
                                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isFunctionHandle()) {
                            Error(L"Nelson:interpreter:invalidFieldAssignment",
                                L"Cannot apply A.field_name = B to non struct-array object A.");
                        }
                        resolveObjectIndices(eval, indices);
                        if (tryAssignStringScalarNDimDirect(base, indices, value)) {
                            stack.push_back(base);
                            return;
                        }
                        alignStructFieldsForSubsetAssign(base, value);
                        normalizeParensAssignmentValueForColon(indices, value);
                        if (tryAssignVectorColon(base, indices, value)) {
                            stack.push_back(base);
                            return;
                        }
                        if (tryAssignEmptyCellColumn(base, indices, value)) {
                            stack.push_back(base);
                            return;
                        }
                        expandColonParensAssignmentIndices(base, indices, value);
                        normalizeParensAssignmentValueForTarget(indices, value);
                        expandClassdefAssignmentBaseWithDefaults(eval, base, indices, value);
                        if (indices.size() == 1) {
                            if (!assignStructVectorSubset(base, indices[0], value)) {
                                base.setVectorSubset(indices[0], value);
                            }
                        } else if (tryAssignRowColonSlice(base, indices, value)) {
                            stack.push_back(base);
                            return;
                        } else {
                            base.setNDimSubset(indices, value);
                        }
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_BRACES_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        if (indices.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        if (base.isClassType() || base.isHandle()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("{}");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction && base.isClassType()) {
                                Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
                            }
                            if (!haveFunction) {
                                checkBracesAssignTarget(base);
                                expandColonContentAssignmentIndices(base, indices);
                                try {
                                    if (indices.size() == 1) {
                                        base.setVectorContentsAsList(indices[0], values);
                                    } else {
                                        base.setNDimContentsAsList(indices, values);
                                    }
                                } catch (const NelsonException& e) {
                                    if (e.message()
                                            == Nelson::resolveMessageTemplate(
                                                L"Nelson:interpreter:illegalZeroNegativeIndex",
                                                L"Illegal zero or negative index")
                                        || e.message()
                                            == resolveMessageTemplate(
                                                L"Nelson:interpreter:cannotConvertStringData",
                                                L"Cannot convert string data types to indices.")) {
                                        Error(L"Nelson:interpreter:notEnoughRhsValues",
                                            L"Not enough right hand side values to satisfy left "
                                            L"hand side expression.");
                                    }
                                    throw;
                                }
                                stack.push_back(base);
                                return;
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        checkBracesAssignTarget(base);
                        resolveObjectIndices(eval, indices);
                        expandColonContentAssignmentIndices(base, indices);
                        try {
                            if (indices.size() == 1) {
                                base.setVectorContentsAsList(indices[0], values);
                            } else {
                                base.setNDimContentsAsList(indices, values);
                            }
                        } catch (const NelsonException& e) {
                            if (e.message()
                                    == Nelson::resolveMessageTemplate(
                                        L"Nelson:interpreter:illegalZeroNegativeIndex",
                                        L"Illegal zero or negative index")
                                || e.message()
                                    == resolveMessageTemplate(
                                        L"Nelson:interpreter:cannotConvertStringData",
                                        L"Cannot convert string data types to indices.")) {
                                Error(L"Nelson:interpreter:notEnoughRhsValues",
                                    L"Not enough right hand side values to satisfy left hand side "
                                    L"expression.");
                            }
                            throw;
                        }
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_DOT_CHAIN_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        if (ins.A >= chunk.constants.size()) {
                            Error(L"Nelson:bytecode:fieldConstantIndexOutOfRange",
                                L"BytecodeVM: field constant index out of range.");
                        }
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf value = pop();
                        const bool hasParentValue = (ins.flags & INST_FLAG_NO_PARENT_VALUE) == 0;
                        ArrayOf parentBase = hasParentValue ? pop() : ArrayOf();
                        ArrayOf base = pop();
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.A));
                        bool isBraceIndex = (ins.flags & INST_FLAG_INDEX_ONLY) != 0;
                        if (base.isClassType() || base.isHandle()) {
                            std::string currentClass
                                = base.isHandle() ? base.handleCategory() : base.classType();
                            bool useClassSpecificSubsasgn = classdefLoadClass(currentClass)
                                && classdefHasMethod(currentClass, SUBSASGN_OPERATOR_STR);
                            if (useClassSpecificSubsasgn) {
                                stringVector subtypes;
                                ArrayOfVector subsindices;
                                subtypes.push_back(".");
                                subsindices.push_back(ArrayOf::characterArray(field));
                                subtypes.push_back(isBraceIndex ? "{}" : "()");
                                subsindices.push_back(classIndexCell(indices));
                                bool haveFunction = false;
                                ArrayOf assigned = eval->bytecodeAssignClass(
                                    base, subtypes, subsindices, value, haveFunction);
                                if (haveFunction) {
                                    stack.push_back(assigned);
                                    return;
                                }
                            }
                        }
                        if (!hasParentValue) {
                            parentBase = readDotValueForAssign(eval, base, field);
                        }
                        ArrayOf assignedParent = isBraceIndex
                            ? assignBracesToValue(eval, parentBase, indices, value)
                            : assignParensToValue(eval, parentBase, indices, value);
                        stack.push_back(assignDotToValue(eval, base, field, assignedParent));
                    });
                } break;
                case OpCode::ASGN_DOTDYN_CHAIN_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf fieldValue = pop();
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        stack.push_back(assignTableDynamicDotIndex(eval, base, fieldValue, indices,
                            value, (ins.flags & INST_FLAG_INDEX_ONLY) != 0));
                    });
                } break;
                case OpCode::SUBIDX_DOTDYN_CHAIN: {
                    ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                    ArrayOf fieldValue = pop();
                    ArrayOf base = pop();
                    stack.push_back(readTableDynamicDotIndex(eval, base, fieldValue, indices));
                } break;
                case OpCode::ASGN_BRACES_MULTI_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf base = pop();
                        if (indices.empty()) {
                            Error(L"Nelson:interpreter:indexExpressionExpected",
                                L"Index expression expected.");
                        }
                        checkBracesAssignTarget(base);
                        resolveObjectIndices(eval, indices);
                        expandColonContentAssignmentIndices(base, indices);
                        indexType targetCount = contentAssignmentTargetCount(indices);
                        indexType valueCount
                            = ins.C == 0 ? targetCount : static_cast<indexType>(ins.C);
                        if (valueCount > static_cast<indexType>(stack.size())) {
                            Error(L"Nelson:interpreter:notEnoughRhsValues",
                                L"Not enough right hand side values to satisfy left hand side "
                                L"expression.");
                        }
                        ArrayOfVector values(static_cast<size_t>(valueCount));
                        values.resize(static_cast<size_t>(valueCount));
                        for (int k = static_cast<int>(valueCount) - 1; k >= 0; --k) {
                            values[static_cast<size_t>(k)] = pop();
                        }
                        if (static_cast<indexType>(values.size()) < targetCount) {
                            Error(L"Nelson:interpreter:notEnoughRhsValues",
                                L"Not enough right hand side values to satisfy left hand side "
                                L"expression.");
                        }
                        while (static_cast<indexType>(values.size()) > targetCount) {
                            values.pop_back();
                        }
                        if (base.isEmpty() && indices.size() == 1
                            && static_cast<indexType>(values.size()) == targetCount) {
                            // Growing an undefined or empty target into a cell.
                            // setVectorContentsAsList honors the actual (possibly non-contiguous
                            // or permuted) subscript positions, leaves gaps as empty cells, and
                            // still yields a 1xN row cell for the contiguous 1:N case.
                            if (base.dataClass() != NLS_CELL_ARRAY
                                && base.dataClass() != NLS_STRING_ARRAY) {
                                base = ArrayOf::emptyCell(Dimensions(0, 0));
                            }
                            base.setVectorContentsAsList(indices[0], values);
                            stack.push_back(base);
                            return;
                        }
                        if (indices.size() == 1) {
                            base.setVectorContentsAsList(indices[0], values);
                        } else {
                            base.setNDimContentsAsList(indices, values);
                        }
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_DOT_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        ArrayOfVector values;
                        values.push_back(value);
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                        if (tryFastTableDotAssign(eval, base, field, value)) {
                            stack.push_back(base);
                            return;
                        }
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArray(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(L"Nelson:interpreter:invalidFieldAssignment",
                                    L"Cannot apply A.field_name = B to non struct-array object A.");
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            stack.push_back(base);
                            return;
                        }
                        checkDotAssignTarget(base);
                        base.setFieldAsList(field, values);
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_DOTDYN_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf fieldValue = pop();
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        if (isIndexingOperationValue(fieldValue)) {
                            stack.push_back(
                                applyIndexingOperationWrite(eval, base, fieldValue, 0, value));
                            return;
                        }
                        if (tryAssignClassDynamicField(eval, base, fieldValue, value)) {
                            stack.push_back(base);
                            return;
                        }
                        std::string field;
                        try {
                            field = fieldValue.asUtf8String();
                        } catch (const NelsonException&) {
                            Error(L"Nelson:interpreter:invalidDynamicFieldName",
                                L"Dynamic field name must be a string.");
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArray(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(L"Nelson:interpreter:invalidFieldAssignment",
                                    L"Cannot apply A.field_name = B to non struct-array object A.");
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            stack.push_back(base);
                            return;
                        }
                        checkDotAssignTarget(base);
                        base.setFieldAsList(field, values);
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_DOT_MULTI_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.A));
                        ArrayOf base = pop();
                        indexType valueCount = static_cast<indexType>(ins.C);
                        if (ins.C == 0) {
                            valueCount = static_cast<indexType>(base.elementCount());
                            if (valueCount == 0) {
                                valueCount = 1;
                            }
                        }
                        if (valueCount > static_cast<indexType>(stack.size())) {
                            Error(L"Nelson:interpreter:notEnoughRhsValues",
                                L"Not enough right hand side values to satisfy left hand side "
                                L"expression.");
                        }
                        ArrayOfVector values = popVector(static_cast<uint16_t>(valueCount));
                        stack.push_back(assignFieldMultiValue(eval, base, field, values));
                    });
                } break;
                case OpCode::ASGN_DOTDYN_MULTI_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf fieldValue = pop();
                        ArrayOf base = pop();
                        std::string field;
                        try {
                            field = fieldValue.asUtf8String();
                        } catch (const NelsonException&) {
                            Error(L"Nelson:interpreter:invalidDynamicFieldName",
                                L"Dynamic field name must be a string.");
                        }
                        indexType valueCount = static_cast<indexType>(ins.C);
                        if (ins.C == 0) {
                            valueCount = static_cast<indexType>(base.elementCount());
                            if (valueCount == 0) {
                                valueCount = 1;
                            }
                        }
                        if (valueCount > static_cast<indexType>(stack.size())) {
                            Error(L"Nelson:interpreter:notEnoughRhsValues",
                                L"Not enough right hand side values to satisfy left hand side "
                                L"expression.");
                        }
                        ArrayOfVector values = popVector(static_cast<uint16_t>(valueCount));
                        stack.push_back(assignFieldMultiValue(eval, base, field, values));
                    });
                } break;
                case OpCode::BUILD_ROW: {
                    ArrayOfVector values = popVector(ins.A);
                    ArrayOf directRow = ArrayOf::doubleScalar(0.);
                    if (tryCommaListScalarDoubleRow(values, directRow)) {
                        stack.push_back(directRow);
                    } else {
                        stack.push_back(eval->horzcatOperator(expandCommaLists(values)));
                    }
                } break;
                case OpCode::BUILD_MATRIX: {
                    ArrayOfVector rows = popVector(ins.A);
                    stack.push_back(eval->vertcatOperator(rows));
                } break;
                case OpCode::BUILD_CELL: {
                    uint16_t nrows = ins.A;
                    uint16_t ncols = ins.B;
                    ArrayOfMatrix matrix(nrows);
                    matrix.resize(nrows);
                    for (int row = static_cast<int>(nrows) - 1; row >= 0; --row) {
                        ArrayOfVector values(ncols);
                        values.resize(ncols);
                        for (int col = static_cast<int>(ncols) - 1; col >= 0; --col) {
                            values[static_cast<size_t>(col)] = pop();
                        }
                        matrix[static_cast<size_t>(row)] = expandCommaLists(values);
                    }
                    stack.push_back(ArrayOf::cellArray(matrix));
                } break;
                case OpCode::BUILD_CELL_FIELD_LIST: {
                    if (ins.B >= chunk.constants.size()) {
                        Error(L"Nelson:bytecode:fieldConstantIndexOutOfRange",
                            L"BytecodeVM: field constant index out of range.");
                    }
                    const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                    ArrayOf base = pop();
                    ArrayOfMatrix matrix(1);
                    if (base.isClassType()) {
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArray(field));
                        bool haveFunction = false;
                        ArrayOfVector result
                            = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                        matrix[0]
                            = haveFunction ? result : getClassdefFieldAsList(eval, base, field);
                    } else if (base.isHandle() || base.isGraphicsObject()) {
                        ArrayOfVector params;
                        matrix[0] = getOrInvokeHandle(eval, base, field, params);
                    } else {
                        matrix[0] = base.fieldAsList(field);
                    }
                    stack.push_back(ArrayOf::cellArray(matrix));
                } break;
                case OpCode::DISPLAY:
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:displayOperandOutOfRange",
                            L"BytecodeVM: display operand out of range.");
                    }
                    eval->display(frame.locals[ins.A], chunk.names[ins.B], false, false);
                    break;
                case OpCode::DISPLAY_STACK:
                    if (stack.empty() || ins.A >= chunk.names.size()) {
                        Error(L"Nelson:bytecode:displayOperandOutOfRange",
                            L"BytecodeVM: display operand out of range.");
                    }
                    if (!isNoOutputSentinel(stack.back())) {
                        eval->display(stack.back(), chunk.names[ins.A], false, false);
                    }
                    break;
                case OpCode::DISPLAY_ANS:
                    if (stack.empty()) {
                        Error(L"Nelson:bytecode:stackUnderflow", L"BytecodeVM: stack underflow.");
                    }
                    if (!isNoOutputSentinel(stack.back())) {
                        eval->display(stack.back(), "ans", false, true);
                    }
                    break;
                case OpCode::RETURN:
                    eval->setState(NLS_STATE_RETURN);
                    clearStatementContext();
                    return;
                case OpCode::ABORT:
                    eval->setState(NLS_STATE_ABORT);
                    clearStatementContext();
                    return;
                case OpCode::QUIT:
                    eval->setState(NLS_STATE_QUIT);
                    clearStatementContext();
                    return;
                default:
                    Error(L"Nelson:bytecode:unhandledOpcode", L"BytecodeVM: unhandled opcode.");
                }
            } catch (const NelsonException& e) {
                if (eval->getState() == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
                    clearStatementContext();
                    throw;
                }
                if (frame.exceptionFrames.empty()) {
                    clearStatementContext();
                    throw;
                }
                ExceptionFrame exceptionFrame = frame.exceptionFrames.back();
                frame.exceptionFrames.pop_back();
                eval->setLastErrorException(e);
                eval->resetState();
                stack.clear();
                frame.endContexts.clear();
                clearStatementContext();
                pc = exceptionFrame.catchPC;
            }

            if (eval->getState() == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
                clearStatementContext();
                return;
            }
        }
        clearStatementContext();
    }
    //=============================================================================
    void
    syncLocalsToContext(Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame)
    {
        Context* context = eval->getContext();
        size_t count = std::min(chunk.localNames.size(), frame.locals.size());
        for (size_t k = 0; k < count; ++k) {
            const std::string& name = chunk.localNames[k];
            if (!name.empty() && isRuntimeAssignedLocalSlot(frame, k)) {
                context->insertVariableLocally(name, frame.locals[k]);
            }
        }
    }
    //=============================================================================
    ArrayOf
    returnValueOrEmpty(
        Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame, size_t retIndex)
    {
        if (retIndex < chunk.retSlots.size() && chunk.retSlots[retIndex] < frame.locals.size()) {
            return frame.locals[chunk.retSlots[retIndex]];
        }
        if (retIndex < chunk.retNames.size()) {
            ArrayOf value;
            if (eval->getContext()->lookupVariable(chunk.retNames[retIndex], value)) {
                return value;
            }
        }
        return ArrayOf::empty();
    }
    //=============================================================================
    bool
    isReturnAssigned(
        Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame, size_t retIndex)
    {
        if (retIndex < chunk.retSlots.size() && chunk.retSlots[retIndex] < frame.locals.size()
            && isRuntimeAssignedLocalSlot(frame, chunk.retSlots[retIndex])) {
            return true;
        }
        if (retIndex < chunk.retNames.size()
            && eval->getContext()->isVariable(chunk.retNames[retIndex])) {
            return true;
        }
        return false;
    }
    //=============================================================================
    ArrayOfVector
    prepareFunctionOutputsFromFrame(
        Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame, int nLhs)
    {
        ArrayOfVector outputs;
        bool hasVarargout = !chunk.retNames.empty() && chunk.retNames.back() == "varargout";
        if (!hasVarargout) {
            if (nLhs > static_cast<int>(chunk.retNames.size())) {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            }
            size_t outputCount = static_cast<size_t>(nLhs < 0 ? 0 : nLhs);
            outputs.reserve(outputCount);
            for (size_t k = 0; k < outputCount; ++k) {
                if (!isReturnAssigned(eval, chunk, frame, k)) {
                    Error("Nelson:interpreter:outputNotAssigned",
                        "Output argument '%s' is not assigned during call to '%s'.",
                        chunk.retNames[k], chunk.functionName);
                }
                outputs.push_back(returnValueOrEmpty(eval, chunk, frame, k));
            }
            return outputs;
        }

        size_t varargoutIndex = chunk.retNames.size() - 1;
        ArrayOf varargout = returnValueOrEmpty(eval, chunk, frame, varargoutIndex);
        bool haveVarargout = varargoutIndex < chunk.retSlots.size()
            && chunk.retSlots[varargoutIndex] < frame.locals.size()
            && isRuntimeAssignedLocalSlot(frame, chunk.retSlots[varargoutIndex]);
        if (!haveVarargout && varargoutIndex < chunk.retNames.size()) {
            haveVarargout = eval->getContext()->isVariable(chunk.retNames[varargoutIndex]);
        }
        if (haveVarargout && varargout.dataClass() != NLS_CELL_ARRAY) {
            Error(L"Nelson:interpreter:invalidVarargout",
                L"The special variable 'varargout' was not defined as a cell-array.");
        }

        indexType varlen = haveVarargout ? varargout.elementCount() : 0;
        int explicitCount = static_cast<int>(chunk.retNames.size()) - 1;
        int requested = nLhs < 0 ? 0 : nLhs;
        // varargout must be a defined cell array only when nargout exceeds the
        // named outputs; nargout 0 still returns the first output (ans).
        if (requested > explicitCount && !haveVarargout) {
            Error(L"Nelson:interpreter:invalidVarargout",
                L"The special variable 'varargout' was not defined as a cell-array.");
        }

        if (explicitCount == 0 && varlen > 0 && nLhs <= 1) {
            outputs.resize(1);
            const ArrayOf* dp = static_cast<const ArrayOf*>(varargout.dataPointer());
            outputs[0] = dp[0];
            outputs[0].name("");
            return outputs;
        }
        if (requested == 0 && explicitCount > 0 && isReturnAssigned(eval, chunk, frame, 0)) {
            requested = 1;
        }
        outputs.resize(static_cast<size_t>(requested));
        int explicitToCopy = std::min(explicitCount, requested);
        for (int k = 0; k < explicitToCopy; ++k) {
            if (!isReturnAssigned(eval, chunk, frame, static_cast<size_t>(k))) {
                Error("Nelson:interpreter:outputNotAssigned",
                    "Output argument '%s' is not assigned during call to '%s'.",
                    chunk.retNames[static_cast<size_t>(k)], chunk.functionName);
            }
            outputs[static_cast<size_t>(k)]
                = returnValueOrEmpty(eval, chunk, frame, static_cast<size_t>(k));
        }
        if (requested > explicitCount) {
            int toFill = requested - explicitCount;
            if (static_cast<indexType>(toFill) > varlen) {
                Error(L"Nelson:interpreter:notEnoughVarargout",
                    L"Not enough outputs in varargout to satisfy call.");
            }
            const ArrayOf* dp = static_cast<const ArrayOf*>(varargout.dataPointer());
            for (int k = 0; k < toFill; ++k) {
                outputs[static_cast<size_t>(explicitCount + k)] = dp[k];
            }
        }
        return outputs;
    }
    //=============================================================================
    // the argument name only feeds inputname: the callee's variable holds the
    // bare value, so copies of it never report the caller's name
    void
    bindInputToLocal(VMCallFrame& frame, size_t slot, const ArrayOf& value)
    {
        frame.locals[slot] = value;
        frame.locals[slot].name("");
        markRuntimeAssigned(frame, slot);
    }
    //=============================================================================
    void
    bindFunctionInputsToFrame(
        const BytecodeChunk& chunk, VMCallFrame& frame, const ArrayOfVector& argIn)
    {
        if (chunk.argNames.empty()) {
            return;
        }

        bool hasVarargin = chunk.argNames.back() == "varargin";
        if (!hasVarargin) {
            size_t count = std::min(argIn.size(), chunk.argNames.size());
            count = std::min(count, frame.locals.size());
            for (size_t k = 0; k < count; ++k) {
                bindInputToLocal(frame, k, argIn[k]);
            }
            return;
        }

        size_t explicitCount = chunk.argNames.size() - 1;
        size_t copied = std::min(argIn.size(), explicitCount);
        copied = std::min(copied, frame.locals.size());
        for (size_t k = 0; k < copied; ++k) {
            bindInputToLocal(frame, k, argIn[k]);
        }

        if (explicitCount >= frame.locals.size()) {
            return;
        }
        size_t vargCount = argIn.size() > explicitCount ? argIn.size() - explicitCount : 0;
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(vargCount);
        auto* dp = static_cast<ArrayOf*>(varg.readWriteDataPointer());
        for (size_t k = 0; k < vargCount; ++k) {
            dp[k] = argIn[explicitCount + k];
            dp[k].name("");
        }
        varg.name("varargin");
        frame.locals[explicitCount] = varg;
        markRuntimeAssigned(frame, explicitCount);
    }
    //=============================================================================
    // Scope-free frames never call Context::pushScope, which is where the
    // recursion limit lives: bound the VM frame depth here so runaway
    // recursion errors out instead of overflowing the C++ stack.
    thread_local size_t vmActiveFrameDepth = 0;
    struct VMFrameDepthGuard
    {
        explicit VMFrameDepthGuard(Evaluator* eval)
        {
            ++vmActiveFrameDepth;
            Context* context = (eval != nullptr) ? eval->getContext() : nullptr;
            if (context != nullptr && vmActiveFrameDepth > context->getRecursionDepth()) {
                --vmActiveFrameDepth;
                Error(L"Nelson:stackDepthExceeded", L"Allowable stack depth exceeded...");
            }
        }
        ~VMFrameDepthGuard() { --vmActiveFrameDepth; }
        VMFrameDepthGuard(const VMFrameDepthGuard&) = delete;
        VMFrameDepthGuard&
        operator=(const VMFrameDepthGuard&)
            = delete;
    };
    //=============================================================================
    ArrayOf
    executeFunctionOneArgOneOutput(
        Evaluator* eval, const BytecodeChunk& chunk, const ArrayOf& argIn, bool callerPushedScope)
    {
        VMFrameDepthGuard depthGuard(eval);
        FrameBufferGuard frameGuard(eval, chunk);
        VMCallFrame& frame = frameGuard.frame();
        frame.init(chunk, 1);
        frame.hasContextScope = callerPushedScope;
        frame.skipDebugStatements = true;
        frame.nArgIn = 1;
        if (!chunk.argNames.empty() && !frame.locals.empty()) {
            bindInputToLocal(frame, 0, argIn);
        }
        applyImportDeclarations(eval, chunk);
        runChunk(eval, frame);
        frameGuard.restoreLazyScope();
        syncNestedFunctionHandlesInFrame(chunk, frame);
        ArrayOf output = returnValueOrEmpty(eval, chunk, frame, 0);
        if (chunk.selfIsNestedMacro) {
            syncLocalsToContext(eval, chunk, frame);
        }
        dropCallerScopeVariablesBeforeFrame(eval, chunk, frame);
        frameGuard.release();
        return output;
    }
    //=============================================================================
} // namespace bcvm_detail
//=============================================================================
using namespace bcvm_detail;
//=============================================================================
bool
BytecodeVM::functionNeedsCallerContext(Evaluator* eval, FunctionDef* funcDef, bool refreshRequired)
{
    const uint64 generation = FunctionsInMemory::getInstance()->generation();
    // Every call site asks this twice per call, so answer straight from the
    // callee chunk cache when it is already valid: the walk below would build
    // its visiting set only to read the very same fields.
    if (funcDef != nullptr && funcDef->type() == NLS_MACRO_FUNCTION) {
        auto* macroDef = dynamic_cast<MacroFunctionDef*>(funcDef);
        if (macroDef != nullptr && !macroDef->getIsScript() && !macroDef->nestedFunction
            && macroDef->bytecodeChunk != nullptr) {
            const BytecodeChunk& calleeChunk = *macroDef->bytecodeChunk;
            if (refreshRequired) {
                if (calleeChunk.callerContextRefreshCacheValid
                    && calleeChunk.callerContextRefreshCacheGeneration == generation) {
                    return calleeChunk.callerContextRefreshRequired;
                }
            } else if (calleeChunk.callerContextSyncCacheValid
                && calleeChunk.callerContextSyncCacheGeneration == generation) {
                return calleeChunk.callerContextSyncRequired;
            }
        }
    }
    std::unordered_set<FunctionDef*> visiting;
    return functionNeedsCallerContextWalk(eval, funcDef, visiting, refreshRequired, generation);
}
//=============================================================================
ArrayOfVector
BytecodeVM::executeFunction(Evaluator* eval, const BytecodeChunk& chunk, const ArrayOfVector& argIn,
    int nLhs, bool callerPushedScope)
{
    VMFrameDepthGuard depthGuard(eval);
    FrameBufferGuard frameGuard(eval, chunk);
    VMCallFrame& frame = frameGuard.frame();
    frame.init(chunk, nLhs);
    frame.hasContextScope = callerPushedScope;
    frame.nArgIn = static_cast<int>(argIn.size());
    bindFunctionInputsToFrame(chunk, frame, argIn);
    applyImportDeclarations(eval, chunk);
    runChunk(eval, frame);
    frameGuard.restoreLazyScope();
    syncNestedFunctionHandlesInFrame(chunk, frame);
    ArrayOfVector outputs = prepareFunctionOutputsFromFrame(eval, chunk, frame, nLhs);
    if (chunk.selfIsNestedMacro) {
        syncLocalsToContext(eval, chunk, frame);
    }
    dropCallerScopeVariablesBeforeFrame(eval, chunk, frame);
    frameGuard.release();
    return outputs;
}
//=============================================================================
void
BytecodeVM::executeScript(Evaluator* eval, const BytecodeChunk& chunk)
{
    FrameBufferGuard frameGuard(eval, chunk);
    VMCallFrame& frame = frameGuard.frame();
    frame.init(chunk, 0);
    try {
        applyImportDeclarations(eval, chunk);
        runChunk(eval, frame);
    } catch (...) {
        try {
            syncLocalsToContext(eval, chunk, frame);
        } catch (...) {
            // Publishing partial script results must not hide its error.
        }
        throw;
    }
    syncLocalsToContext(eval, chunk, frame);
    frameGuard.release();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
