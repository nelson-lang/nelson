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
#include <unordered_map>
#include <unordered_set>
#include "AnonymousMacroFunctionDef.hpp"
#include "BuiltInFunctionDef.hpp"
#include "BytecodeVM.hpp"
#include "CallbackQueue.hpp"
#include "CheckIfWhileCondition.hpp"
#include "ClassdefParser.hpp"
#include "ClearHandle.hpp"
#include "Context.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "EventQueue.hpp"
#include "Exception.hpp"
#include "FunctionDef.hpp"
#include "IsValidVariableName.hpp"
#include "MacroFunctionDef.hpp"
#include "MException.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonPrint.hpp"
#include "Operators.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "Scope.hpp"
#include "VMCallFrame.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
//=============================================================================
#define BYTECODE_VM_CLASSDEF_ACCESSOR_GET "get"
#define BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY "Cannot get property: "
#define BYTECODE_VM_CLASSDEF_ERROR_NO_SUCH_PROPERTY "No such property: "
#define BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER "Undefined classdef member: "
#define BYTECODE_VM_ERROR_CLASSDEF_MEMBER_CONSTANT_INDEX                                           \
    "BytecodeVM: classdef member constant index out of range."
    //=============================================================================
    constexpr const char* NO_OUTPUT_SENTINEL_NAME = "__nelson_bytecode_no_output__";
    constexpr const char* COMMA_LIST_SENTINEL_NAME = "__nelson_bytecode_comma_list__";
    //=============================================================================
    struct FrameBuffer
    {
        const BytecodeChunk* chunk = nullptr;
        bool inUse = false;
        std::vector<ArrayOf> locals;
        std::vector<uint8_t> assignedLocals;
        std::vector<ArrayOf> valueStack;
        std::vector<GenericLoopState> loopStates;
        std::vector<DoubleLoopState> dblLoops;
        std::vector<RangeLoopState> rangeLoops;
        std::vector<ExceptionFrame> exceptionFrames;
        std::vector<EndContext> endContexts;
    };
    //=============================================================================
    FrameBuffer*
    acquireFrameBuffer(const BytecodeChunk& chunk)
    {
        static thread_local std::vector<std::unique_ptr<FrameBuffer>> buffers;
        for (std::unique_ptr<FrameBuffer>& buffer : buffers) {
            if (!buffer->inUse && buffer->chunk == &chunk) {
                buffer->inUse = true;
                return buffer.get();
            }
        }
        buffers.emplace_back(std::make_unique<FrameBuffer>());
        FrameBuffer* buffer = buffers.back().get();
        buffer->chunk = &chunk;
        buffer->inUse = true;
        return buffer;
    }
    //=============================================================================
    void
    attachFrameBuffer(VMCallFrame& frame, FrameBuffer* buffer)
    {
        if (buffer == nullptr) {
            return;
        }
        frame.locals.swap(buffer->locals);
        frame.assignedLocals.swap(buffer->assignedLocals);
        frame.valueStack.swap(buffer->valueStack);
        frame.loopStates.swap(buffer->loopStates);
        frame.dblLoops.swap(buffer->dblLoops);
        frame.rangeLoops.swap(buffer->rangeLoops);
        frame.exceptionFrames.swap(buffer->exceptionFrames);
        frame.endContexts.swap(buffer->endContexts);
    }
    //=============================================================================
    void
    releaseFrameBuffer(VMCallFrame& frame, FrameBuffer* buffer)
    {
        if (buffer == nullptr) {
            return;
        }
        frame.locals.clear();
        frame.assignedLocals.clear();
        frame.valueStack.clear();
        frame.loopStates.clear();
        frame.dblLoops.clear();
        frame.rangeLoops.clear();
        frame.exceptionFrames.clear();
        frame.endContexts.clear();
        frame.locals.swap(buffer->locals);
        frame.assignedLocals.swap(buffer->assignedLocals);
        frame.valueStack.swap(buffer->valueStack);
        frame.loopStates.swap(buffer->loopStates);
        frame.dblLoops.swap(buffer->dblLoops);
        frame.rangeLoops.swap(buffer->rangeLoops);
        frame.exceptionFrames.swap(buffer->exceptionFrames);
        frame.endContexts.swap(buffer->endContexts);
        buffer->inUse = false;
    }
    //=============================================================================
    ArrayOf
    noOutputSentinel()
    {
        ArrayOf value = ArrayOf::emptyConstructor();
        value.name(NO_OUTPUT_SENTINEL_NAME);
        return value;
    }
    //=============================================================================
    ArrayOf
    logicalConstructorFromKeyword(bool value, const ArrayOfVector& args)
    {
        Dimensions dim;
        indexType idxMax = static_cast<indexType>(args.size());
        if (args.empty()) {
            dim = Dimensions(1, 1);
        } else {
            if (args.size() >= 2) {
                indexType pos = static_cast<indexType>(args.size() - 2);
                if (args[pos].isRowVectorCharacterArray() || args[pos].isScalarStringArray()) {
                    std::wstring arg = args[pos].getContentAsWideString();
                    if (arg != L"like") {
                        Error(
                            _W("Wrong value for argument #") + std::to_wstring(pos + 1) + _W("."));
                    }
                    ArrayOf likeValue = args[pos + 1];
                    if (likeValue.getDataClass() != NLS_LOGICAL) {
                        Error(_W("Input following 'like' is not a logical array."));
                    }
                    idxMax = pos;
                    if (idxMax == 0) {
                        dim[0] = 1;
                        dim[1] = 1;
                    }
                }
            }
            for (indexType k = 0; k < idxMax; ++k) {
                dim[k] = args[k].getContentAsScalarIndex();
            }
            if (idxMax == 1) {
                dim[1] = dim[0];
            }
            dim.simplify();
        }

        logical* data = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.getElementCount(), stringVector(), true));
        std::memset(data, value ? 1 : 0, sizeof(logical) * dim.getElementCount());
        return ArrayOf(NLS_LOGICAL, dim, data, false);
    }
    //=============================================================================
    void
    declareGlobalVariable(Context* context, const std::string& name)
    {
        if (!IsValidVariableName(name)) {
            Error(_W("Argument must contain a valid variable name."));
        }
        if (context->isLockedVariable(name)) {
            Error(_W("variable is locked."));
        }
        context->addGlobalVariable(name);
    }
    //=============================================================================
    void
    declarePersistentVariable(Context* context, const std::string& name, bool isScriptChunk)
    {
        if (isScriptChunk || context->getCurrentScope()->getName() == "base") {
            Error(_W("A 'persistent' declaration is only allowed in a script file function."));
        }
        if (!IsValidVariableName(name)) {
            Error(_W("Argument must contain a valid variable name."));
        }
        if (context->isLockedVariable(name)) {
            Error(_W("variable is locked."));
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
            value = ArrayOf::doubleConstructor(context->getCurrentScope()->getNargIn());
            return true;
        }
        if (name == "nargout") {
            value = ArrayOf::doubleConstructor(context->getCurrentScope()->getNargOut());
            return true;
        }
        return false;
    }
    //=============================================================================
    class OutputProbeInterface : public Interface
    {
    private:
        Interface* wrapped = nullptr;
        bool didOutput = false;

    public:
        explicit OutputProbeInterface(Interface* wrappedInterface) : wrapped(wrappedInterface) { }

        bool
        hasOutput() const
        {
            return didOutput;
        }

        std::string
        getLine(const std::string& prompt) override
        {
            return wrapped->getLine(prompt);
        }

        std::wstring
        getLine(const std::wstring& prompt) override
        {
            return wrapped->getLine(prompt);
        }

        std::wstring
        getInput(const std::wstring& prompt) override
        {
            return wrapped->getInput(prompt);
        }

        size_t
        getTerminalWidth() override
        {
            return wrapped->getTerminalWidth();
        }

        size_t
        getTerminalHeight() override
        {
            return wrapped->getTerminalHeight();
        }

        void
        outputMessage(const std::string& msg) override
        {
            didOutput = true;
            wrapped->outputMessage(msg);
        }

        void
        outputMessage(const std::wstring& msg) override
        {
            didOutput = true;
            wrapped->outputMessage(msg);
        }

        void
        errorMessage(const std::string& msg) override
        {
            didOutput = true;
            wrapped->errorMessage(msg);
        }

        void
        errorMessage(const std::wstring& msg) override
        {
            didOutput = true;
            wrapped->errorMessage(msg);
        }

        void
        warningMessage(const std::string& msg) override
        {
            didOutput = true;
            wrapped->warningMessage(msg);
        }

        void
        warningMessage(const std::wstring& msg) override
        {
            didOutput = true;
            wrapped->warningMessage(msg);
        }

        void
        clearTerminal() override
        {
            wrapped->clearTerminal();
        }

        bool
        isAtPrompt() override
        {
            return wrapped->isAtPrompt();
        }

        void
        interruptGetLineByEvent() override
        {
            wrapped->interruptGetLineByEvent();
        }
    };
    //=============================================================================
    class ScopedOutputProbe
    {
    private:
        Evaluator* eval = nullptr;
        Interface* savedInterface = nullptr;
        OutputProbeInterface probe;

    public:
        explicit ScopedOutputProbe(Evaluator* evaluator)
            : eval(evaluator), savedInterface(evaluator->getInterface()), probe(savedInterface)
        {
            eval->setInterface(&probe);
            setPrintInterface(&probe);
        }

        ~ScopedOutputProbe()
        {
            eval->setInterface(savedInterface);
            setPrintInterface(savedInterface);
        }

        bool
        hasOutput() const
        {
            return probe.hasOutput();
        }
    };
    //=============================================================================
    bool
    isMacroFunctionWithVarargout(FunctionDef* fdef)
    {
        return fdef != nullptr
            && (fdef->type() == NLS_MACRO_FUNCTION || fdef->type() == NLS_ANONYMOUS_MACRO_FUNCTION)
            && fdef->outputArgCount() == -1;
    }
    //=============================================================================
    bool
    astContainsIdentifier(AbstractSyntaxTreePtr node, const std::string& name)
    {
        while (node != nullptr) {
            if (node->type == id_node && node->text == name) {
                return true;
            }
            if (astContainsIdentifier(node->down, name)) {
                return true;
            }
            node = node->right;
        }
        return false;
    }
    //=============================================================================
    bool
    astContainsVarargoutIndexedByNargout(AbstractSyntaxTreePtr node)
    {
        while (node != nullptr) {
            if (node->type == id_node && node->text == "varargout" && node->down != nullptr
                && node->down->opNum == OP_BRACES
                && astContainsIdentifier(node->down->down, "nargout")) {
                return true;
            }
            if (astContainsVarargoutIndexedByNargout(node->down)) {
                return true;
            }
            node = node->right;
        }
        return false;
    }
    //=============================================================================
    bool
    shouldRetrySilentVarargoutCall(FunctionDef* fdef)
    {
        if (!isMacroFunctionWithVarargout(fdef) || fdef->type() != NLS_MACRO_FUNCTION) {
            return false;
        }
        auto* macroDef = reinterpret_cast<MacroFunctionDef*>(fdef);
        return astContainsVarargoutIndexedByNargout(macroDef->code);
    }
    //=============================================================================
    ArrayOfVector
    callFunctionHandle(Evaluator* eval, const ArrayOf& handle, const ArrayOfVector& args, int nLhs);
    //=============================================================================
    ArrayOfVector
    evaluateFunctionAndDetectOutput(
        Evaluator* eval, FunctionDef* fdef, const ArrayOfVector& args, int nout, bool& hasOutput)
    {
        ScopedOutputProbe probe(eval);
        ArrayOfVector result = fdef->evaluateFunction(eval, args, nout);
        hasOutput = probe.hasOutput();
        return result;
    }
    //=============================================================================
    ArrayOfVector
    callFunctionHandleAndDetectOutput(Evaluator* eval, const ArrayOf& handle,
        const ArrayOfVector& args, int nout, bool& hasOutput)
    {
        ScopedOutputProbe probe(eval);
        ArrayOfVector result = callFunctionHandle(eval, handle, args, nout);
        hasOutput = probe.hasOutput();
        return result;
    }
    //=============================================================================
    ArrayOf
    commaListSentinel(const ArrayOfVector& values)
    {
        ArrayOfMatrix matrix;
        matrix.push_back(values);
        ArrayOf value = ArrayOf::cellConstructor(matrix);
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
    bool
    splitClassdefMemberSpec(
        const std::string& spec, std::string& className, std::string& memberName)
    {
        size_t pos = spec.find_last_of('.');
        if (pos == std::string::npos || pos == 0 || pos + 1 >= spec.size()) {
            return false;
        }
        className = spec.substr(0, pos);
        memberName = spec.substr(pos + 1);
        return true;
    }
    //=============================================================================
    void
    checkClassdefPropertyGet(Evaluator* eval, const ArrayOf& base, const std::string& field)
    {
        if (!base.isClassType()) {
            return;
        }
        const std::string className = base.getClassType();
        auto* manager = ClassdefDefinitionManager::getInstance();
        if (!manager->loadClass(className)) {
            return;
        }
        if (!manager->hasProperty(className, field)) {
            Error(_(BYTECODE_VM_CLASSDEF_ERROR_NO_SUCH_PROPERTY) + field);
        }
        if (!manager->canGetProperty(className, field,
                eval == nullptr ? std::string() : eval->getClassdefAccessContext())) {
            Error(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field);
        }
    }
    //=============================================================================
    ArrayOfVector
    getClassdefDependentFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field)
    {
        const std::string className = base.getClassType();
        auto* manager = ClassdefDefinitionManager::getInstance();
        if (!manager->hasDependentProperty(className, field)) {
            return {};
        }
        checkClassdefPropertyGet(eval, base, field);
        if (eval == nullptr || eval->getContext() == nullptr) {
            Error(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field);
        }
        std::string functionName;
        if (!manager->resolvePropertyAccessorFunction(
                className, field, BYTECODE_VM_CLASSDEF_ACCESSOR_GET, functionName)) {
            Error(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field);
        }
        FunctionDef* functionDef = nullptr;
        if (!eval->getContext()->lookupFunction(functionName, functionDef)
            || functionDef == nullptr) {
            Error(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field);
        }

        ArrayOfVector values;
        const indexType elementCount = base.getElementCount();
        values.reserve(static_cast<size_t>(elementCount));
        for (indexType k = 0; k < elementCount; ++k) {
            ArrayOf object = base;
            if (elementCount != 1) {
                ArrayOf mutableBase = base;
                ArrayOf index = ArrayOf::doubleConstructor(static_cast<double>(k + 1));
                object = mutableBase.getVectorSubset(index);
            }
            ArrayOfVector inputs;
            inputs << object;
            ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, 1);
            values << (result.empty() ? ArrayOf::emptyConstructor() : result[0]);
        }
        return values;
    }
    //=============================================================================
    ArrayOfVector
    getClassdefFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field)
    {
        checkClassdefPropertyGet(eval, base, field);
        if (base.isClassType()) {
            ArrayOfVector dependentValues = getClassdefDependentFieldAsList(eval, base, field);
            if (!dependentValues.empty()
                || ClassdefDefinitionManager::getInstance()->hasDependentProperty(
                    base.getClassType(), field)) {
                return dependentValues;
            }
        }
        return base.getFieldAsList(field);
    }
    //=============================================================================
    ArrayOf
    classIndexCell(const ArrayOfVector& indices)
    {
        ArrayOfMatrix matrix;
        matrix.push_back(indices);
        return ArrayOf::cellConstructor(matrix);
    }
    //=============================================================================
    bool
    isCommaListSentinel(const ArrayOf& value)
    {
        return value.name() == COMMA_LIST_SENTINEL_NAME;
    }
    //=============================================================================
    ArrayOfVector
    commaListValues(ArrayOf value)
    {
        ArrayOf index = ArrayOf::characterArrayConstructor(":");
        return value.getVectorContentsAsList(index);
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
    isColonIndex(ArrayOf index)
    {
        if ((index.getDataClass() == NLS_CHAR && index.getElementCount() == 1)
            || (index.isStringArray() && index.isScalar())) {
            try {
                return index.getContentAsWideString() == L":";
            } catch (const Exception&) {
                return false;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    checkShortcutScalarCondition(const ArrayOf& value, bool isAnd)
    {
        if (!value.isScalar()) {
            Error(isAnd
                    ? _W("Operand to && operator must be convertible to logical scalar values.")
                    : _W("Operand to || operator must be convertible to logical scalar values."));
        }
        return checkIfWhileCondition(value);
    }
    //=============================================================================
    void
    expandColonContentAssignmentIndices(const ArrayOf& base, ArrayOfVector& indices)
    {
        if (indices.size() == 1) {
            if (isColonIndex(indices[0])) {
                indices[0] = ArrayOf::integerRangeConstructor(1, 1, base.getElementCount(), false);
            }
            return;
        }
        for (size_t k = 0; k < indices.size(); ++k) {
            if (isColonIndex(indices[k])) {
                indices[k] = ArrayOf::integerRangeConstructor(
                    1, 1, base.getDimensionLength(static_cast<int>(k)), false);
            }
        }
    }
    //=============================================================================
    void
    expandColonParensAssignmentIndices(
        const ArrayOf& base, ArrayOfVector& indices, const ArrayOf& value)
    {
        if (base.isEmpty() && indices.size() > 1) {
            size_t colonCount = 0;
            for (const ArrayOf& index : indices) {
                if (isColonIndex(index)) {
                    ++colonCount;
                }
            }
            for (size_t k = 0; k < indices.size(); ++k) {
                if (isColonIndex(indices[k])) {
                    indexType len = value.getDimensionLength(static_cast<int>(k));
                    if (colonCount == 1 && value.isVector()) {
                        indexType fixedCount = 1;
                        for (size_t j = 0; j < indices.size(); ++j) {
                            if (j != k) {
                                fixedCount *= indices[j].getElementCount();
                            }
                        }
                        indexType valueCount = value.getElementCount();
                        if (fixedCount > 0 && valueCount % fixedCount == 0) {
                            len = valueCount / fixedCount;
                        } else if (len == 0) {
                            len = valueCount;
                        }
                    }
                    indices[k] = ArrayOf::integerRangeConstructor(1, 1, len, false);
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
        if (indices.size() < 2 || value.getElementCount() == 0 || !value.isVector()) {
            return;
        }
        Dimensions targetDims;
        indexType targetCount = 1;
        for (size_t k = 0; k < indices.size(); ++k) {
            indexType len = indices[k].getElementCount();
            targetDims[static_cast<indexType>(k)] = len;
            targetCount *= len;
        }
        if (targetCount == value.getElementCount() && !value.getDimensions().equals(targetDims)) {
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
            count *= index.getElementCount();
        }
        return count;
    }
    //=============================================================================
    uint16_t
    outputCountFromSpec(const ArrayOf& spec)
    {
        indexType count = spec.getElementCount();
        if (count > std::numeric_limits<uint16_t>::max()) {
            Error(_W("Too many output arguments."));
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
        if (!indices[0].isScalar() || value.getElementCount() == 0) {
            return;
        }
        Dimensions dims(1, value.getElementCount());
        value.reshape(dims);
    }
    //=============================================================================
    bool
    tryAssignRowColonSlice(ArrayOf& base, ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() != 2 || !isColonIndex(indices[1]) || !indices[0].isScalar()) {
            return false;
        }
        indexType nRows = base.getDimensionLength(0);
        indexType nCols = base.getDimensionLength(1);
        if (value.getElementCount() != nCols) {
            return false;
        }
        indexType row = static_cast<indexType>(indices[0].getContentAsInteger64Scalar());
        ArrayOf linear
            = ArrayOf::integerRangeConstructor(row, nRows, row + (nCols - 1) * nRows, false);
        base.setVectorSubset(linear, value);
        return true;
    }
    //=============================================================================
    bool
    tryAssignVectorColon(ArrayOf& base, ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() != 1 || !isColonIndex(indices[0])) {
            return false;
        }
        if (value.isEmpty()) {
            bool wasSparse = base.isSparse();
            base = ArrayOf::emptyConstructor();
            if (wasSparse) {
                base.makeSparse();
            }
            return true;
        }
        if (value.getElementCount() == base.getElementCount()
            && value.getDimensions().equals(base.getDimensions())) {
            base.setValue(value);
            return true;
        }
        ArrayOf linear = ArrayOf::integerRangeConstructor(1, 1, base.getElementCount(), false);
        base.setVectorSubset(linear, value);
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
            if (indices[1].getContentAsInteger64Scalar() != 1) {
                return false;
            }
        } catch (const Exception&) {
            return false;
        }
        base.setValue(value);
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleScalar2D(ArrayOf& base, const ArrayOfVector& indices, ArrayOf& value)
    {
        if (indices.size() != 2 || base.getDataClass() != NLS_DOUBLE || base.isSparse()
            || !indices[0].isScalar() || !indices[1].isScalar() || !value.isScalar()) {
            return false;
        }
        indexType row = 0;
        indexType col = 0;
        double scalar = 0.0;
        try {
            row = static_cast<indexType>(indices[0].getContentAsInteger64Scalar());
            col = static_cast<indexType>(indices[1].getContentAsInteger64Scalar());
            scalar = value.getContentAsDoubleScalar();
        } catch (const Exception&) {
            return false;
        }
        if (row < 1 || col < 1 || row > base.getDimensionLength(0)
            || col > base.getDimensionLength(1)) {
            return false;
        }
        indexType offset = (row - 1) + (col - 1) * base.getDimensionLength(0);
        static_cast<double*>(base.getReadWriteDataPointer())[offset] = scalar;
        return true;
    }
    //=============================================================================
    bool
    tryAssignDoubleScalar2DDirect(
        ArrayOf& base, const ArrayOf& rowIndex, const ArrayOf& colIndex, const ArrayOf& value)
    {
        if (base.getDataClass() != NLS_DOUBLE || base.isSparse() || !rowIndex.isScalar()
            || !colIndex.isScalar() || !value.isScalar()) {
            return false;
        }
        indexType row = 0;
        indexType col = 0;
        double scalar = 0.0;
        try {
            row = static_cast<indexType>(rowIndex.getContentAsInteger64Scalar());
            col = static_cast<indexType>(colIndex.getContentAsInteger64Scalar());
            scalar = value.getContentAsDoubleScalar();
        } catch (const Exception&) {
            return false;
        }
        if (row < 1 || col < 1 || row > base.getDimensionLength(0)
            || col > base.getDimensionLength(1)) {
            return false;
        }
        indexType offset = (row - 1) + (col - 1) * base.getDimensionLength(0);
        static_cast<double*>(base.getReadWriteDataPointer())[offset] = scalar;
        return true;
    }
    //=============================================================================
    ArrayOf
    constantAsArray(const ConstantValue& value)
    {
        if (const auto* arrayValue = std::get_if<ArrayOf>(&value)) {
            return *arrayValue;
        }
        Error(_W("Bytecode constant is not an ArrayOf value."));
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
    void
    detachFunctionArguments(ArrayOfVector& args)
    {
        for (auto& arg : args) {
            arg.ensureSingleOwner();
        }
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
    void
    markRuntimeUnassigned(VMCallFrame& frame, size_t slot)
    {
        if (slot < frame.assignedLocals.size()) {
            frame.assignedLocals[slot] = 0;
        }
    }
    //=============================================================================
    void
    pollInterrupt(Evaluator* eval)
    {
        uint64 id = eval->getID();
        NelsonConfiguration* config = NelsonConfiguration::getInstance();
        if (!config->getInterruptPending(id)) {
            return;
        }
        if (id == 0) {
            config->setInterruptPending(false, id);
            CallbackQueue::getInstance()->clear();
            EventQueue::getInstance()->clear();
            eval->setState(NLS_STATE_ABORT);
            Error(MSG_CTRL_C_DETECTED);
        }
        Error(_W("Execution of the future was cancelled."),
            L"parallel:fevalqueue:ExecutionCancelled");
    }
    //=============================================================================
    ArrayOfVector
    callFunctionHandle(Evaluator* eval, const ArrayOf& handle, const ArrayOfVector& args, int nLhs)
    {
        if (!handle.isFunctionHandle()) {
            Error(_W("Argument #1 must be a valid function_handle."));
        }
        function_handle fh = handle.getContentAsFunctionHandle();
        FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
        if (funcDef == nullptr) {
            Error(_W("Function does not exist."));
        }
        if (funcDef->type() == NLS_ANONYMOUS_MACRO_FUNCTION) {
            auto* anonymousDef = reinterpret_cast<AnonymousMacroFunctionDef*>(funcDef);
            if (anonymousDef->isStatelessSimpleIdentity() && args.size() == 1 && nLhs <= 1) {
                if (nLhs == 0) {
                    return {};
                }
                ArrayOfVector result;
                result.push_back(args[0]);
                return result;
            }
        }
        return funcDef->evaluateFunction(eval, args, nLhs);
    }
    //=============================================================================
    stringVector
    splitAnonymousArguments(const std::string& payload)
    {
        stringVector values;
        std::string current;
        for (char ch : payload) {
            if (ch == '\x1f') {
                values.push_back(current);
                current.clear();
            } else {
                current.push_back(ch);
            }
        }
        if (!current.empty() || !payload.empty()) {
            values.push_back(current);
        }
        return values;
    }
    //=============================================================================
    ArrayOf
    makeAnonymousHandle(Evaluator* eval, VMCallFrame& frame, const std::string& content,
        const stringVector& arguments)
    {
        stringVector variableNames;
        std::vector<ArrayOf> variables;

        auto addCapture = [&](const std::string& name, const ArrayOf& value) {
            if (name.empty()
                || std::find(variableNames.begin(), variableNames.end(), name)
                    != variableNames.end()) {
                return;
            }
            variableNames.push_back(name);
            variables.push_back(value);
        };

        size_t localCount = std::min(frame.chunk->localNames.size(), frame.locals.size());
        for (size_t k = 0; k < localCount; ++k) {
            if (!isAssignedLocalSlot(*frame.chunk, k)) {
                continue;
            }
            addCapture(frame.chunk->localNames[k], frame.locals[k]);
        }

        Scope* scope = eval->getContext()->getCurrentScope();
        if (scope != nullptr) {
            stringVector scopeNames;
            scope->getVariablesList(false, scopeNames);
            for (const std::string& name : scopeNames) {
                ArrayOf value;
                if (scope->lookupVariable(name, value)) {
                    addCapture(name, value);
                }
            }
        }

        AnonymousMacroFunctionDef* functionDef = nullptr;
        try {
            functionDef
                = new AnonymousMacroFunctionDef(content, arguments, variableNames, variables);
        } catch (std::bad_alloc&) {
            functionDef = nullptr;
        } catch (const Exception&) {
            delete functionDef;
            functionDef = nullptr;
        }
        if (functionDef == nullptr) {
            Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
        }
        function_handle handle;
        handle.anonymousHandle = reinterpret_cast<nelson_handle*>(functionDef);
        return ArrayOf::functionHandleConstructor(handle);
    }
    //=============================================================================
    void
    collectCurrentFrameAndScopeVariables(Evaluator* eval, VMCallFrame& frame,
        stringVector& variableNames, std::vector<ArrayOf>& variables)
    {
        auto addCapture = [&](const std::string& name, const ArrayOf& value) {
            if (name.empty()
                || std::find(variableNames.begin(), variableNames.end(), name)
                    != variableNames.end()) {
                return;
            }
            variableNames.push_back(name);
            variables.push_back(value);
        };

        size_t localCount = std::min(frame.chunk->localNames.size(), frame.locals.size());
        for (size_t k = 0; k < localCount; ++k) {
            if (frame.chunk->localNames[k].empty()) {
                continue;
            }
            addCapture(frame.chunk->localNames[k], frame.locals[k]);
        }

        Scope* scope = eval->getContext()->getCurrentScope();
        if (scope != nullptr) {
            stringVector scopeNames;
            scope->getVariablesList(false, scopeNames);
            for (const std::string& name : scopeNames) {
                ArrayOf value;
                if (scope->lookupVariable(name, value)) {
                    addCapture(name, value);
                }
            }
        }
    }
    //=============================================================================
    void
    syncNestedFunctionHandlesInFrame(const BytecodeChunk& chunk, VMCallFrame& frame)
    {
        if (!chunk.mayCreateNamedFunctionHandle) {
            return;
        }
        std::vector<AnonymousMacroFunctionDef*> nestedHandles;
        const size_t localCount = std::min(frame.locals.size(), chunk.localNames.size());
        nestedHandles.reserve(2);
        for (size_t k = 0; k < localCount; ++k) {
            if (frame.locals[k].getDataClass() != NLS_FUNCTION_HANDLE) {
                continue;
            }
            function_handle fh = frame.locals[k].getContentAsFunctionHandle();
            if (fh.anonymousHandle == nullptr) {
                continue;
            }
            auto* functionDef = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
            if (functionDef != nullptr && functionDef->isBoundNestedFunctionHandle()) {
                nestedHandles.push_back(functionDef);
            }
        }
        if (nestedHandles.empty()) {
            return;
        }

        stringVector names;
        std::vector<ArrayOf> values;
        names.reserve(localCount);
        values.reserve(localCount);
        for (size_t k = 0; k < localCount; ++k) {
            if (chunk.localNames[k].empty()) {
                continue;
            }
            names.push_back(chunk.localNames[k]);
            values.push_back(frame.locals[k]);
        }

        for (auto* functionDef : nestedHandles) {
            functionDef->syncCapturedVariables(names, values);
        }
    }
    //=============================================================================
    ArrayOf
    makeNamedHandle(Evaluator* eval, VMCallFrame& frame, const std::string& name)
    {
        FunctionDef* localFunctionDef = nullptr;
        Scope* currentScope = eval->getContext()->getCurrentScope();
        if (currentScope != nullptr && currentScope->lookupFunction(name, localFunctionDef)
            && localFunctionDef != nullptr && localFunctionDef->type() == NLS_MACRO_FUNCTION) {
            auto* macroDef = dynamic_cast<MacroFunctionDef*>(localFunctionDef);
            if (macroDef != nullptr && macroDef->nestedFunction) {
                stringVector variableNames;
                std::vector<ArrayOf> variables;
                collectCurrentFrameAndScopeVariables(eval, frame, variableNames, variables);
                AnonymousMacroFunctionDef* functionDef = nullptr;
                try {
                    functionDef
                        = new AnonymousMacroFunctionDef(name, macroDef, variableNames, variables);
                } catch (std::bad_alloc&) {
                    functionDef = nullptr;
                } catch (const Exception&) {
                    delete functionDef;
                    functionDef = nullptr;
                }
                if (functionDef == nullptr) {
                    Error(_("A valid function name expected."),
                        "Nelson:dispatcher:invalidFunctionName");
                }
                function_handle handle;
                handle.anonymousHandle = reinterpret_cast<nelson_handle*>(functionDef);
                return ArrayOf::functionHandleConstructor(handle);
            }
        }

        AnonymousMacroFunctionDef* functionDef = nullptr;
        try {
            functionDef = new AnonymousMacroFunctionDef(name);
        } catch (std::bad_alloc&) {
            functionDef = nullptr;
        } catch (const Exception&) {
            delete functionDef;
            functionDef = nullptr;
        }
        if (functionDef == nullptr) {
            Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
        }
        function_handle handle;
        handle.anonymousHandle = reinterpret_cast<nelson_handle*>(functionDef);
        return ArrayOf::functionHandleConstructor(handle);
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
            && a.getElementCount() == 1 && b.getElementCount() == 1) {
            double av = a.getContentAsDoubleScalar();
            double bv = b.getContentAsDoubleScalar();
            switch (op) {
            case OpCode::OP_PLUS:
                return ArrayOf::doubleConstructor(av + bv);
            case OpCode::OP_MINUS:
                return ArrayOf::doubleConstructor(av - bv);
            case OpCode::OP_TIMES:
            case OpCode::OP_MTIMES:
                return ArrayOf::doubleConstructor(av * bv);
            case OpCode::OP_RDIV:
            case OpCode::OP_MRDIV:
                return ArrayOf::doubleConstructor(av / bv);
            case OpCode::OP_LT:
                return ArrayOf::logicalConstructor(av < bv);
            case OpCode::OP_LEQ:
                return ArrayOf::logicalConstructor(av <= bv);
            case OpCode::OP_GT:
                return ArrayOf::logicalConstructor(av > bv);
            case OpCode::OP_GEQ:
                return ArrayOf::logicalConstructor(av >= bv);
            case OpCode::OP_EQ:
                return ArrayOf::logicalConstructor(av == bv);
            case OpCode::OP_NEQ:
                return ArrayOf::logicalConstructor(av != bv);
            default:
                break;
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
                Error(_W("BytecodeVM: unsupported binary operator."));
            }
            return ArrayOf::emptyConstructor();
        });
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
                Error(_W("BytecodeVM: unsupported ternary operator."));
            }
            return ArrayOf::emptyConstructor();
        });
    }
    //=============================================================================
    ArrayOf
    unaryOperator(Evaluator* eval, OpCode op, const ArrayOf& a)
    {
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
                Error(_W("BytecodeVM: unsupported unary operator."));
            }
            return ArrayOf::emptyConstructor();
        });
    }
    //=============================================================================
    ArrayOf
    loopValueAt(GenericLoopState& state)
    {
        if (state.isRowVector) {
            switch (state.indexSet.getDataClass()) {
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
                return state.indexSet.getValueAtIndex(static_cast<uint64>(state.current));
            default: {
                ArrayOf index = ArrayOf::doubleConstructor(static_cast<double>(state.current + 1));
                return state.indexSet.getVectorSubset(index);
            }
            }
        }

        indexType rows = state.indexSet.getRows();
        ArrayOfVector indices;
        indices.reserve(2);
        indices.push_back(ArrayOf::integerRangeConstructor(1, 1, rows, false));
        indices.push_back(ArrayOf::doubleConstructor(static_cast<double>(state.current + 1)));
        return state.indexSet.getNDimSubset(indices);
    }
    //=============================================================================
    void
    initGenericLoop(VMCallFrame& frame, uint16_t loopSlot, uint16_t varSlot, ArrayOf indexSet,
        uint32_t& pc, uint16_t emptyTarget)
    {
        if (loopSlot >= frame.loopStates.size() || varSlot >= frame.locals.size()) {
            Error(_W("BytecodeVM: for-loop operand out of range."));
        }
        GenericLoopState& state = frame.loopStates[loopSlot];
        state.indexSet = indexSet;
        state.isRowVector = state.indexSet.isRowVector();
        state.current = 0;
        if (state.indexSet.getElementCount() == 0) {
            state.count = 0;
        } else {
            state.count = state.isRowVector
                ? state.indexSet.getElementCount()
                : (state.indexSet.isColumnVector() ? 1 : state.indexSet.getColumns());
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
            Error(_W("BytecodeVM: for-loop operand out of range."));
        }
        GenericLoopState& state = frame.loopStates[loopSlot];
        state.indexSet = indexSet;
        state.isRowVector = state.indexSet.isRowVector();
        state.current = 0;
        if (state.indexSet.getElementCount() == 0) {
            state.count = 0;
        } else {
            state.count = state.isRowVector
                ? state.indexSet.getElementCount()
                : (state.indexSet.isColumnVector() ? 1 : state.indexSet.getColumns());
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
            Error(_W("BytecodeVM: for-loop operand out of range."));
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
            Error(_W("BytecodeVM: for-loop operand out of range."));
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
            Error(_W("BytecodeVM: local operand out of range."));
        }
        ArrayOf& target = locals[slot];
        if (!target.isDoubleType(true) || target.getElementCount() != 1) {
            target = ArrayOf::doubleConstructor(value);
            return;
        }
        *static_cast<double*>(target.getReadWriteDataPointer()) = value;
    }
    //=============================================================================
    void
    alignStructFieldsForSubsetAssign(ArrayOf& base, const ArrayOf& value)
    {
        if (!base.isStruct() || !value.isStruct()) {
            return;
        }
        stringVector baseFields = base.getFieldNames();
        stringVector valueFields = value.getFieldNames();
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

    bool
    assignStructVectorSubset(ArrayOf& base, ArrayOf index, ArrayOf value)
    {
        if (!base.isStruct() || !value.isStruct()) {
            return false;
        }

        alignStructFieldsForSubsetAssign(base, value);
        value.promoteType(NLS_STRUCT_ARRAY, base.getFieldNames());
        base.setVectorSubset(index, value);
        return true;
    }
    //=============================================================================
    void
    handleDebugStatement(Evaluator* eval, VMCallFrame& frame, const Instruction& ins)
    {
        const BytecodeChunk& chunk = *frame.chunk;
        if (ins.A >= chunk.spans.size()) {
            return;
        }
        const SourceSpan& span = chunk.spans[ins.A];
        size_t line = static_cast<size_t>(span.line);
        size_t maxLine = span.endLine == 0 ? line : static_cast<size_t>(span.endLine);
        if (!eval->onBytecodeBreakpoint(chunk.sourcePath, chunk.functionName, line, maxLine)) {
            return;
        }

        eval->bpActive = true;
        while (true) {
            eval->debugCLI();
            State state = eval->getState();
            if (state == NLS_STATE_ABORT) {
                eval->bpActive = false;
                break;
            }
            if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
                eval->bpActive = false;
                eval->resetDebugDepth();
                break;
            }
            if (state == NLS_STATE_DEBUG_QUIT_ALL || state == NLS_STATE_DEBUG_QUIT) {
                eval->bpActive = false;
                eval->setState(NLS_STATE_ABORT);
                break;
            }
            if (state == NLS_STATE_DEBUG_CONTINUE || state == NLS_STATE_DEBUG_STEP) {
                eval->bpActive = (state == NLS_STATE_DEBUG_STEP);
                eval->setState(NLS_STATE_OK);
                break;
            }
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

        auto pop = [&]() -> ArrayOf {
            if (stack.empty()) {
                Error(_W("BytecodeVM: stack underflow."));
            }
            ArrayOf value = stack.back();
            stack.pop_back();
            return value;
        };

        auto popVector = [&](uint16_t count) -> ArrayOfVector {
            ArrayOfVector values(count);
            values.resize(count);
            for (int k = static_cast<int>(count) - 1; k >= 0; --k) {
                values[static_cast<size_t>(k)] = pop();
            }
            return values;
        };

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
                    frame.locals[k] = ArrayOf::emptyConstructor();
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
        auto processStatementBoundaryQueues = [&]() {
            if ((++statementBoundaryCounter & 63U) == 0) {
                if (eval->haveEventsLoop()) {
                    ProcessEventsDynamicFunctionWithoutWait();
                }
            }
            if (!eval->commandQueue.isEmpty()) {
                syncAssignedLocalsToContext();
                std::wstring command;
                eval->commandQueue.get(command);
                eval->evaluateString(command);
                refreshLocalsFromContext();
            }
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

        auto evaluateDotReference = [&](const ArrayOf& base, const std::string& field,
                                        const ArrayOfVector& params) -> ArrayOfVector {
            if (base.isClassType()) {
                stringVector subtypes;
                ArrayOfVector subsindices;
                subtypes.push_back(".");
                subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                if (!params.empty()) {
                    subtypes.push_back("()");
                    subsindices.push_back(classIndexCell(params));
                }
                bool haveFunction = false;
                ArrayOfVector result
                    = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                if (!haveFunction) {
                    if (!params.empty()) {
                        ArrayOfVector methodResult;
                        if (eval->bytecodeInvokeObjectMethodIfExists(
                                base, field, params, 1, methodResult)) {
                            return methodResult;
                        }
                    }
                    return getClassdefFieldAsList(eval, base, field);
                }
                return result;
            }
            if (base.isHandle() || base.isGraphicsObject()) {
                return getOrInvokeHandle(eval, base, field, params);
            }
            if (!base.isStruct()) {
                Error(_W("Invalid indexing."));
            }
            ArrayOfVector fieldValues = base.getFieldAsList(field);
            if (!params.empty()) {
                ArrayOf fieldValue
                    = fieldValues.empty() ? ArrayOf::emptyConstructor() : fieldValues[0];
                ArrayOfVector result;
                ArrayOfVector mutableParams = params;
                if (params.size() == 1) {
                    result.push_back(fieldValue.getVectorSubset(mutableParams[0]));
                } else {
                    result.push_back(fieldValue.getNDimSubset(mutableParams));
                }
                return result;
            }
            return fieldValues;
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

        std::unordered_map<FunctionDef*, bool> callerContextSyncCache;
        std::unordered_map<FunctionDef*, bool> callerContextRefreshCache;
        auto functionHasCallerContextRequirement
            = [&](FunctionDef* funcDef, std::unordered_set<FunctionDef*>& visiting,
                  bool refreshRequired, const auto& self) -> bool {
            if (funcDef == nullptr) {
                return true;
            }
            auto& cache = refreshRequired ? callerContextRefreshCache : callerContextSyncCache;
            auto cached = cache.find(funcDef);
            if (cached != cache.end()) {
                return cached->second;
            }
            bool result = true;
            if (funcDef->type() == NLS_BUILT_IN_FUNCTION) {
                auto* builtinDef = dynamic_cast<BuiltInFunctionDef*>(funcDef);
                if (builtinDef == nullptr) {
                    return true;
                }
                result = refreshRequired ? builtinDef->mustRefreshCallerContext()
                                         : builtinDef->mustSyncCallerContext();
                cache.emplace(funcDef, result);
                return result;
            }
            BytecodeChunk* calleeChunkPtr = nullptr;
            if (funcDef->type() == NLS_MACRO_FUNCTION) {
                auto* macroDef = dynamic_cast<MacroFunctionDef*>(funcDef);
                if (macroDef == nullptr || macroDef->getIsScript()) {
                    return true;
                }
                if (macroDef->nestedFunction) {
                    cache.emplace(funcDef, true);
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
                    cache.emplace(funcDef, false);
                    return false;
                }
                if (anonymousDef->isFunctionHandle()) {
                    FunctionDef* targetDef = nullptr;
                    if (!eval->lookupFunction(anonymousDef->getContent(), targetDef)) {
                        return true;
                    }
                    result = self(targetDef, visiting, refreshRequired, self);
                    cache.emplace(funcDef, result);
                    return result;
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
            if (refreshRequired && calleeChunkPtr->callerContextRefreshCacheValid) {
                result = calleeChunkPtr->callerContextRefreshRequired;
                cache.emplace(funcDef, result);
                return result;
            }
            if (!refreshRequired && calleeChunkPtr->callerContextSyncCacheValid) {
                result = calleeChunkPtr->callerContextSyncRequired;
                cache.emplace(funcDef, result);
                return result;
            }
            result = false;
            const BytecodeChunk& calleeChunk = *calleeChunkPtr;
            for (const Instruction& calleeIns : calleeChunk.code) {
                if (calleeIns.op != OpCode::CALL_NAMED && calleeIns.op != OpCode::MAKE_FH_NAMED) {
                    continue;
                }
                if (calleeIns.A >= calleeChunk.names.size()) {
                    return true;
                }
                FunctionDef* nestedDef = nullptr;
                if (!eval->lookupFunction(calleeChunk.names[calleeIns.A], nestedDef)) {
                    return true;
                }
                if (self(nestedDef, visiting, refreshRequired, self)) {
                    result = true;
                    break;
                }
            }
            if (refreshRequired) {
                calleeChunkPtr->callerContextRefreshRequired = result;
                calleeChunkPtr->callerContextRefreshCacheValid = true;
            } else {
                calleeChunkPtr->callerContextSyncRequired = result;
                calleeChunkPtr->callerContextSyncCacheValid = true;
            }
            cache.emplace(funcDef, result);
            return result;
        };

        auto needsCallerContextSync = [&](FunctionDef* funcDef) -> bool {
            std::unordered_set<FunctionDef*> visiting;
            return functionHasCallerContextRequirement(
                funcDef, visiting, false, functionHasCallerContextRequirement);
        };

        auto needsCallerContextRefresh = [&](FunctionDef* funcDef) -> bool {
            std::unordered_set<FunctionDef*> visiting;
            return functionHasCallerContextRequirement(
                funcDef, visiting, true, functionHasCallerContextRequirement);
        };

        auto functionDefFromHandle = [](const ArrayOf& handle) -> FunctionDef* {
            if (!handle.isFunctionHandle()) {
                return nullptr;
            }
            function_handle fh = handle.getContentAsFunctionHandle();
            return reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
        };

        uint32_t loopBackedgeCounter = 0;
        auto pollLoopBackedge = [&]() {
            if ((++loopBackedgeCounter & 31U) == 0) {
                pollInterrupt(eval);
            }
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
        while (pc < chunk.code.size()) {
            const Instruction& ins = chunk.code[pc++];
            try {
                switch (ins.op) {
                case OpCode::NOP:
                    break;
                case OpCode::DEBUG_STMT:
                    setStatementContext(ins);
                    processStatementBoundaryQueues();
                    handleDebugStatement(eval, frame, ins);
                    break;
                case OpCode::CHECK_INTERRUPT: {
                    pollInterrupt(eval);
                } break;
                case OpCode::LOAD_CONST:
                    stack.push_back(constantAsArray(chunk.constants.get(ins.A)));
                    break;
                case OpCode::LOAD_TRUE:
                    stack.push_back(ArrayOf::logicalConstructor(true));
                    break;
                case OpCode::LOAD_FALSE:
                    stack.push_back(ArrayOf::logicalConstructor(false));
                    break;
                case OpCode::LOAD_EMPTY:
                    stack.push_back(ArrayOf::emptyConstructor());
                    break;
                case OpCode::LOAD_EMPTY_CELL: {
                    ArrayOf value(ArrayOf::emptyConstructor());
                    value.promoteType(NLS_CELL_ARRAY);
                    stack.push_back(value);
                } break;
                case OpCode::MAKE_FH_NAMED: {
                    if (ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    stack.push_back(makeNamedHandle(eval, frame, chunk.names[ins.A]));
                } break;
                case OpCode::MAKE_FH_ANONYMOUS: {
                    const auto& content = std::get<std::string>(chunk.constants.get(ins.A));
                    const auto& argsPayload = std::get<std::string>(chunk.constants.get(ins.B));
                    stack.push_back(makeAnonymousHandle(
                        eval, frame, content, splitAnonymousArguments(argsPayload)));
                } break;
                case OpCode::DECLARE_GLOBAL:
                case OpCode::DECLARE_PERSISTENT: {
                    if (ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    if (ins.op == OpCode::DECLARE_GLOBAL) {
                        declareGlobalVariable(context, chunk.names[ins.A]);
                    } else {
                        declarePersistentVariable(context, chunk.names[ins.A], chunk.isScript);
                    }
                } break;
                case OpCode::LOAD_LOCAL:
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    if (!isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()) {
                            ArrayOf value;
                            if (context->lookupVariable(chunk.localNames[ins.A], value)) {
                                stack.push_back(value);
                                break;
                            }
                            if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) == 0
                                && tryLoadImplicitArgumentCount(
                                    context, chunk.localNames[ins.A], value)) {
                                stack.push_back(value);
                                break;
                            }
                            if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) == 0) {
                                Error(_("Undefined variable: ") + chunk.localNames[ins.A]);
                            }
                        }
                        stack.push_back(ArrayOf::emptyConstructor());
                        break;
                    }
                    stack.push_back(frame.locals[ins.A]);
                    break;
                case OpCode::LOAD_LOCAL_COUNT_RANGE: {
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    ArrayOf value;
                    if (isRuntimeAssignedLocalSlot(frame, ins.A)) {
                        value = frame.locals[ins.A];
                    } else if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->lookupVariable(chunk.localNames[ins.A], value)) {
                    } else {
                        value = ArrayOf::emptyConstructor();
                    }
                    stack.push_back(
                        ArrayOf::integerRangeConstructor(1, 1, value.getElementCount(), false));
                } break;
                case OpCode::STORE_LOCAL:
                case OpCode::ASSIGN:
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()
                        && context->isLockedVariable(chunk.localNames[ins.A])) {
                        Error(_W("Redefining permanent variable."));
                    }
                    if (ins.op == OpCode::ASSIGN) {
                        profiledVoidOperation(
                            eval, SUBSASGN_OPERATOR_STR, [&]() { frame.locals[ins.A] = pop(); });
                    } else {
                        frame.locals[ins.A] = pop();
                    }
                    markRuntimeAssigned(frame, ins.A);
                    break;
                case OpCode::LOAD_GLOBAL: {
                    if (ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    const std::string& name = chunk.names[ins.A];
                    ArrayOf value;
                    if (!context->lookupVariable(name, value)) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            stack.push_back(ArrayOf::emptyConstructor());
                            break;
                        }
                        FunctionDef* fdef = nullptr;
                        if (eval->lookupFunction(name, fdef)) {
                            try {
                                ArrayOfVector result = fdef->evaluateFunction(eval, {}, 1);
                                stack.push_back(
                                    result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                            } catch (const Exception& e) {
                                if (e.getMessage() != _W("Wrong number of output arguments.")
                                    && e.getMessage()
                                        != _W("Not enough outputs in varargout to satisfy call.")) {
                                    throw;
                                }
                                (void)fdef->evaluateFunction(eval, {}, 0);
                                stack.push_back(noOutputSentinel());
                            }
                        } else {
                            Error(_("Undefined variable: ") + name);
                        }
                    } else {
                        stack.push_back(value);
                    }
                } break;
                case OpCode::LOAD_GLOBAL_COUNT_RANGE: {
                    if (ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    ArrayOf value;
                    if (!context->lookupVariable(chunk.names[ins.A], value)) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) == 0) {
                            Error(_("Undefined variable: ") + chunk.names[ins.A]);
                        }
                        value = ArrayOf::emptyConstructor();
                    }
                    stack.push_back(
                        ArrayOf::integerRangeConstructor(1, 1, value.getElementCount(), false));
                } break;
                case OpCode::LOAD_ANS: {
                    ArrayOf value;
                    if (!tryGetAssignedLocalByName("ans", value)
                        && !context->lookupVariable("ans", value)) {
                        value = ArrayOf::emptyConstructor();
                    }
                    stack.push_back(value);
                } break;
                case OpCode::STORE_GLOBAL: {
                    if (ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    const std::string& name = chunk.names[ins.A];
                    ArrayOf value = pop();
                    if (!context->insertVariable(name, value)) {
                        if (context->isLockedVariable(name)) {
                            Error(_W("Redefining permanent variable."));
                        }
                        Error(_W("Valid variable name expected."));
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
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    if (ins.A < chunk.localNames.size() && !chunk.localNames[ins.A].empty()) {
                        context->deleteVariable(chunk.localNames[ins.A]);
                    }
                    frame.locals[ins.A] = ArrayOf::emptyConstructor();
                    markRuntimeUnassigned(frame, ins.A);
                    break;
                case OpCode::CLEAR_LOCAL:
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
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
                    frame.locals[ins.A] = ArrayOf::emptyConstructor();
                    markRuntimeUnassigned(frame, ins.A);
                    break;
                case OpCode::POP:
                    (void)pop();
                    break;
                case OpCode::DUP:
                    if (stack.empty()) {
                        Error(_W("BytecodeVM: stack underflow."));
                    }
                    stack.push_back(stack.back());
                    break;
                case OpCode::SWAP:
                    if (stack.size() < 2) {
                        Error(_W("BytecodeVM: stack underflow."));
                    }
                    std::swap(stack[stack.size() - 1], stack[stack.size() - 2]);
                    break;
                case OpCode::SET_NAME:
                    if (stack.empty() || ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name operand out of range."));
                    }
                    stack.back().name(chunk.names[ins.A]);
                    break;
                case OpCode::PUSH_END_CTX: {
                    if (ins.A >= stack.size()) {
                        Error(_W("BytecodeVM: end context stack underflow."));
                    }
                    const ArrayOf& base = stack[stack.size() - 1 - ins.A];
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
                        Error(ERROR_END_ILLEGAL);
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
                    if (!checkIfWhileCondition(condition)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_TRUE: {
                    ArrayOf condition = pop();
                    if (checkIfWhileCondition(condition)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_FALSE_SCALAR: {
                    ArrayOf condition = pop();
                    if (!checkShortcutScalarCondition(condition, true)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::JUMP_IF_TRUE_SCALAR: {
                    ArrayOf condition = pop();
                    if (checkShortcutScalarCondition(condition, false)) {
                        if (ins.A < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.A;
                    }
                } break;
                case OpCode::CHECK_SCALAR_AND: {
                    ArrayOf condition = pop();
                    stack.push_back(
                        ArrayOf::logicalConstructor(checkShortcutScalarCondition(condition, true)));
                } break;
                case OpCode::CHECK_SCALAR_OR: {
                    ArrayOf condition = pop();
                    stack.push_back(ArrayOf::logicalConstructor(
                        checkShortcutScalarCondition(condition, false)));
                } break;
                case OpCode::CASE_MATCH: {
                    ArrayOf caseValue = pop();
                    ArrayOf switchValue = pop();
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
                        ArrayOf::logicalConstructor(switchValue.testForCaseMatch(caseValue)));
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
                            Error(_W("BytecodeVM: name index out of range."));
                        }
                        context->insertVariable(
                            chunk.names[ins.A], ExceptionToArrayOf(eval->getLastErrorException()));
                        break;
                    }
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    frame.locals[ins.A] = ExceptionToArrayOf(eval->getLastErrorException());
                    markRuntimeAssigned(frame, ins.A);
                    break;
                case OpCode::WHILE_CHECK: {
                    ArrayOf condition = pop();
                    if (!checkIfWhileCondition(condition)) {
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
                        Error(_W("BytecodeVM: double for-loop operand out of range."));
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
                    state.dataPtr = static_cast<const double*>(state.indexSet.getDataPointer());
                    state.current = 0;
                    state.count = state.indexSet.getElementCount();
                    if (state.count == 0) {
                        pc = ins.C;
                    } else if (isGlobal) {
                        context->insertVariable(
                            chunk.names[ins.B], ArrayOf::doubleConstructor(state.dataPtr[0]));
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
                        Error(_W("BytecodeVM: double for-loop operand out of range."));
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
                                ArrayOf::doubleConstructor(state.dataPtr[state.current]));
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
                        Error(_W("BytecodeVM: range for-loop operand out of range."));
                    }
                    RangeLoopState& state = frame.rangeLoops[ins.A];
                    state.stop = pop().getContentAsDoubleScalar();
                    state.step = (ins.flags & INST_FLAG_RANGE_HAS_STEP)
                        ? pop().getContentAsDoubleScalar()
                        : 1.0;
                    state.start = pop().getContentAsDoubleScalar();
                    state.current = state.start;
                    profiledVoidOperation(eval, COLON_OPERATOR_STR, []() { });
                    if (state.step == 0.0 || (state.step > 0.0 && state.current > state.stop)
                        || (state.step < 0.0 && state.current < state.stop)) {
                        pc = ins.C;
                    } else if (isGlobal) {
                        context->insertVariable(
                            chunk.names[ins.B], ArrayOf::doubleConstructor(state.current));
                    } else {
                        setLocalDouble(frame.locals, ins.B, state.current);
                        markRuntimeAssigned(frame, ins.B);
                    }
                } break;
                case OpCode::FOR_STEP_RANGE: {
                    bool isGlobal = (ins.flags & INST_FLAG_GLOBAL) != 0;
                    if (ins.A >= frame.rangeLoops.size()
                        || (!isGlobal && ins.B >= frame.locals.size())
                        || (isGlobal && ins.B >= chunk.names.size())) {
                        Error(_W("BytecodeVM: range for-loop operand out of range."));
                    }
                    RangeLoopState& state = frame.rangeLoops[ins.A];
                    state.current += state.step;
                    bool more = state.step > 0.0 ? state.current <= state.stop
                                                 : state.current >= state.stop;
                    if (more) {
                        if (isGlobal) {
                            context->insertVariable(
                                chunk.names[ins.B], ArrayOf::doubleConstructor(state.current));
                        } else {
                            setLocalDouble(frame.locals, ins.B, state.current);
                            markRuntimeAssigned(frame, ins.B);
                        }
                        if (ins.C < pc) {
                            pollLoopBackedge();
                        }
                        pc = ins.C;
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
                case OpCode::CALL_NAMED: {
                    if (ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    const std::string& name = chunk.names[ins.A];
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    detachFunctionArguments(args);
                    ArrayOf variable;
                    bool hasVariable = tryGetAssignedLocalByName(name, variable);
                    if (!hasVariable) {
                        hasVariable = context->lookupVariable(name, variable);
                    }
                    if (hasVariable && variable.isFunctionHandle()) {
                        FunctionDef* handleDef = functionDefFromHandle(variable);
                        bool syncCallerContext = needsCallerContextSync(handleDef);
                        bool refreshCallerContext = needsCallerContextRefresh(handleDef);
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
                                } catch (const Exception& e) {
                                    if (e.getMessage() != _W("Wrong number of output arguments.")
                                        && e.getMessage()
                                            != _W(
                                                "No output arguments are allowed if only two input "
                                                "arguments.")
                                        && e.getMessage()
                                            != _W("Not enough outputs in varargout to satisfy "
                                                  "call.")) {
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
                                    eval->display(result[k],
                                        result[k].name().empty() ? "ans" : result[k].name(), false,
                                        true);
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
                        } catch (const Exception& e) {
                            if (!(ins.flags & INST_FLAG_PRINT) || nout != 1
                                || (e.getMessage() != _W("Wrong number of output arguments.")
                                    && e.getMessage()
                                        != _W("No output arguments are allowed if only two input "
                                              "arguments.")
                                    && e.getMessage()
                                        != _W(
                                            "Not enough outputs in varargout to satisfy call."))) {
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
                                ArrayOf value
                                    = k < result.size() ? result[k] : ArrayOf::emptyConstructor();
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
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 0 && hasVariable
                        && !args.empty()) {
                        ArrayOf result;
                        if (args.size() == 1) {
                            result = variable.getVectorSubset(args[0]);
                        } else {
                            result = variable.getNDimSubset(args);
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
                            stack.push_back(variable.getVectorSubset(args[0]));
                        } else {
                            stack.push_back(variable.getNDimSubset(args));
                        }
                        break;
                    }
                    FunctionDef* fdef = nullptr;
                    if (!hasVariable && name == chunk.functionName && chunk.selfFunction != nullptr
                        && !chunk.selfFunction->isOverload()) {
                        fdef = chunk.selfFunction;
                    } else if (!eval->lookupFunction(name, fdef)) {
                        if (!args.empty() && (args[0].isHandle() || args[0].isClassType())) {
                            ArrayOf receiver = args[0];
                            ArrayOfVector methodArgs;
                            for (size_t k = 1; k < args.size(); ++k) {
                                methodArgs.push_back(args[k]);
                            }
                            ArrayOfVector result;
                            if (eval->bytecodeInvokeObjectMethodIfExists(
                                    receiver, name, methodArgs, static_cast<int>(ins.C), result)) {
                                for (uint16_t k = 0; k < ins.C; ++k) {
                                    stack.push_back(k < result.size()
                                            ? result[k]
                                            : ArrayOf::emptyConstructor());
                                }
                                break;
                            }
                        }
                        Error(_("Undefined variable or function: ") + name);
                    }
                    bool syncCallerContext = needsCallerContextSync(fdef);
                    bool refreshCallerContext = needsCallerContextRefresh(fdef);
                    if (syncCallerContext) {
                        syncAssignedLocalsToContext();
                    }
                    uint16_t nout = ins.C;
                    if ((ins.flags & INST_FLAG_PRINT) != 0 && nout == 0
                        && (fdef->type() == NLS_MACRO_FUNCTION
                            || fdef->type() == NLS_ANONYMOUS_MACRO_FUNCTION)
                        && fdef->outputArgCount() > 0) {
                        nout = 1;
                    }
                    ArrayOfVector result;
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
                            } catch (const Exception& e) {
                                if (e.getMessage() != _W("Wrong number of output arguments.")
                                    && e.getMessage()
                                        != _W("No output arguments are allowed if only two input "
                                              "arguments.")
                                    && e.getMessage()
                                        != _W("Not enough outputs in varargout to satisfy call.")) {
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
                        } catch (const Exception& e) {
                            if (!(ins.flags & INST_FLAG_PRINT) || nout != 1
                                || (e.getMessage() != _W("Wrong number of output arguments.")
                                    && e.getMessage()
                                        != _W("No output arguments are allowed if only two input "
                                              "arguments.")
                                    && e.getMessage()
                                        != _W(
                                            "Not enough outputs in varargout to satisfy call."))) {
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
                                eval->display(result[k],
                                    result[k].name().empty() ? "ans" : result[k].name(), false,
                                    true);
                            }
                        }
                        break;
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        if ((ins.flags & INST_FLAG_PRINT) != 0 && ins.C == 1 && result.empty()) {
                            stack.push_back(noOutputSentinel());
                        } else {
                            ArrayOf value
                                = k < result.size() ? result[k] : ArrayOf::emptyConstructor();
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
                        Error(_W(BYTECODE_VM_ERROR_CLASSDEF_MEMBER_CONSTANT_INDEX));
                    }
                    const std::string& spec = std::get<std::string>(chunk.constants.get(ins.A));
                    std::string className;
                    std::string memberName;
                    if (!splitClassdefMemberSpec(spec, className, memberName)) {
                        Error(_(BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER) + spec);
                    }

                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    detachFunctionArguments(args);
                    const bool explicitCall = (ins.flags & INST_FLAG_INDEX_ONLY) != 0;
                    const uint8_t outputFlags = ins.flags & ~INST_FLAG_INDEX_ONLY;

                    if (className.find('.') == std::string::npos) {
                        ArrayOf variable;
                        bool hasVariable = tryGetAssignedLocalByName(className, variable);
                        if (!hasVariable) {
                            hasVariable = context->lookupVariable(className, variable);
                        }
                        if (hasVariable) {
                            ArrayOfVector result = evaluateDotReference(variable, memberName, args);
                            for (uint16_t k = 0; k < ins.C; ++k) {
                                stack.push_back(
                                    k < result.size() ? result[k] : ArrayOf::emptyConstructor());
                            }
                            break;
                        }
                    }

                    std::string functionName;
                    auto* manager = ClassdefDefinitionManager::getInstance();
                    bool resolved = false;
                    if (explicitCall) {
                        resolved = manager->resolveStaticMethodFunction(
                            className, memberName, functionName, eval->getClassdefAccessContext());
                    } else if (args.empty()) {
                        resolved = manager->resolveConstantPropertyFunction(className, memberName,
                                       functionName, eval->getClassdefAccessContext())
                            || manager->resolveEnumerationMemberFunction(
                                className, memberName, functionName);
                    }

                    if (!resolved) {
                        if (className.find('.') == std::string::npos) {
                            FunctionDef* baseDef = nullptr;
                            if (eval->lookupFunction(className, baseDef)) {
                                ArrayOfVector baseResult = baseDef->evaluateFunction(eval, {}, 1);
                                ArrayOf base = baseResult.empty() ? ArrayOf::emptyConstructor()
                                                                  : baseResult[0];
                                ArrayOfVector result = evaluateDotReference(base, memberName, args);
                                for (uint16_t k = 0; k < ins.C; ++k) {
                                    stack.push_back(k < result.size()
                                            ? result[k]
                                            : ArrayOf::emptyConstructor());
                                }
                                break;
                            }
                        }
                        Error(_(BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER) + spec);
                    }

                    FunctionDef* fdef = nullptr;
                    if (!eval->lookupFunction(functionName, fdef) || fdef == nullptr) {
                        Error(_(BYTECODE_VM_CLASSDEF_ERROR_UNDEFINED_MEMBER) + spec);
                    }
                    bool syncCallerContext = needsCallerContextSync(fdef);
                    bool refreshCallerContext = needsCallerContextRefresh(fdef);
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
                    } catch (const Exception& e) {
                        if ((outputFlags & INST_FLAG_PRINT) == 0 || nout != 1
                            || (e.getMessage() != _W("Wrong number of output arguments.")
                                && e.getMessage()
                                    != _W("Not enough outputs in varargout to satisfy call."))) {
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
                                eval->display(result[k],
                                    result[k].name().empty() ? "ans" : result[k].name(), false,
                                    true);
                            }
                        }
                        break;
                    }
                    for (uint16_t k = 0; k < nout; ++k) {
                        if ((outputFlags & INST_FLAG_PRINT) != 0 && ins.C == 1 && result.empty()) {
                            stack.push_back(noOutputSentinel());
                        } else {
                            ArrayOf value
                                = k < result.size() ? result[k] : ArrayOf::emptyConstructor();
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
                        Error(_W("BytecodeVM: name index out of range."));
                    }
                    ArrayOf outputSpec = pop();
                    uint16_t nout = outputCountFromSpec(outputSpec);
                    const std::string& name = chunk.names[ins.A];
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    detachFunctionArguments(args);
                    ArrayOf variable;
                    bool hasVariable = tryGetAssignedLocalByName(name, variable);
                    if (!hasVariable) {
                        hasVariable = context->lookupVariable(name, variable);
                    }
                    if (hasVariable && variable.isFunctionHandle()) {
                        FunctionDef* handleDef = functionDefFromHandle(variable);
                        bool syncCallerContext = needsCallerContextSync(handleDef);
                        bool refreshCallerContext = needsCallerContextRefresh(handleDef);
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
                            stack.push_back(
                                k < result.size() ? result[k] : ArrayOf::emptyConstructor());
                        }
                        break;
                    }
                    FunctionDef* fdef = nullptr;
                    if (!eval->lookupFunction(name, fdef)) {
                        Error(_("Undefined variable or function: ") + name);
                    }
                    bool syncCallerContext = needsCallerContextSync(fdef);
                    bool refreshCallerContext = needsCallerContextRefresh(fdef);
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
                        stack.push_back(
                            k < result.size() ? result[k] : ArrayOf::emptyConstructor());
                    }
                } break;
                case OpCode::CALL_BUILTIN:
                case OpCode::CALL_MACRO: {
                    FunctionDef* funcDef = std::get<FunctionDef*>(chunk.constants.get(ins.A));
                    if (funcDef == nullptr) {
                        Error(_W("BytecodeVM: invalid function constant."));
                    }
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    detachFunctionArguments(args);
                    bool syncCallerContext = needsCallerContextSync(funcDef);
                    bool refreshCallerContext = needsCallerContextRefresh(funcDef);
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
                        stack.push_back(
                            k < result.size() ? result[k] : ArrayOf::emptyConstructor());
                    }
                } break;
                case OpCode::CALL_HANDLE: {
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    detachFunctionArguments(args);
                    ArrayOf handle = pop();
                    FunctionDef* handleDef = functionDefFromHandle(handle);
                    bool syncCallerContext = needsCallerContextSync(handleDef);
                    bool refreshCallerContext = needsCallerContextRefresh(handleDef);
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
                        stack.push_back(
                            k < result.size() ? result[k] : ArrayOf::emptyConstructor());
                    }
                } break;
                case OpCode::CALL_HANDLE_DYNAMIC: {
                    ArrayOf outputSpec = pop();
                    uint16_t nout = outputCountFromSpec(outputSpec);
                    ArrayOfVector args = expandCommaLists(popVector(ins.B));
                    detachFunctionArguments(args);
                    ArrayOf handle = pop();
                    FunctionDef* handleDef = functionDefFromHandle(handle);
                    bool syncCallerContext = needsCallerContextSync(handleDef);
                    bool refreshCallerContext = needsCallerContextRefresh(handleDef);
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
                        stack.push_back(
                            k < result.size() ? result[k] : ArrayOf::emptyConstructor());
                    }
                } break;
                case OpCode::SUBIDX_PARENS: {
                    ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                    ArrayOf base = pop();
                    if (ins.C == 1) {
                        stack.push_back(logicalConstructorFromKeyword(
                            base.getContentAsLogicalScalar(), indices));
                        break;
                    }
                    if (base.isFunctionHandle()) {
                        if ((ins.flags & INST_FLAG_INDEX_ONLY) && indices.size() == 1
                            && indices[0].isScalar()
                            && indices[0].getContentAsInteger64Scalar() == 1) {
                            if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                                Error(_W(
                                    "Cannot apply A.field_name = B to non struct-array object A."));
                            }
                            Error(_W("Invalid indexing."));
                        }
                        int nout = (ins.flags & INST_FLAG_QUIET) ? 0 : 1;
                        ArrayOfVector result;
                        FunctionDef* handleDef = functionDefFromHandle(base);
                        bool syncCallerContext = needsCallerContextSync(handleDef);
                        bool refreshCallerContext = needsCallerContextRefresh(handleDef);
                        if (syncCallerContext) {
                            syncAssignedLocalsToContext();
                        }
                        size_t clearEpoch = context->getVariablesClearedEpoch();
                        try {
                            result = callFunctionHandle(eval, base, indices, nout);
                        } catch (const Exception& e) {
                            if (!(ins.flags & INST_FLAG_PRINT) || nout != 1
                                || (e.getMessage() != _W("Wrong number of output arguments.")
                                    && e.getMessage()
                                        != _W(
                                            "Not enough outputs in varargout to satisfy call."))) {
                                throw;
                            }
                            nout = 0;
                            result = callFunctionHandle(eval, base, indices, nout);
                        }
                        refreshAfterVariableClear(clearEpoch);
                        if (refreshCallerContext) {
                            refreshLocalsFromContext();
                        }
                        stack.push_back(nout == 0
                                ? noOutputSentinel()
                                : (result.empty() ? ArrayOf::emptyConstructor() : result[0]));
                        break;
                    }
                    if (base.isClassType()) {
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back("()");
                        subsindices.push_back(classIndexCell(indices));
                        bool haveFunction = false;
                        ArrayOfVector result
                            = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                        if (!haveFunction) {
                            if (ClassdefDefinitionManager::getInstance()->loadClass(
                                    base.getClassType())) {
                                if (indices.empty()) {
                                    Error(_W("Index expression expected."));
                                }
                                if (indices.size() == 1) {
                                    stack.push_back(base.getVectorSubset(indices[0]));
                                } else {
                                    stack.push_back(base.getNDimSubset(indices));
                                }
                                break;
                            }
                            Error(_W("Invalid indexing."));
                        }
                        stack.push_back(result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                        break;
                    }
                    try {
                        if (indices.empty()) {
                            Error(_W("Index expression expected."));
                        }
                        if (indices.size() == 1) {
                            stack.push_back(base.getVectorSubset(indices[0]));
                        } else {
                            stack.push_back(base.getNDimSubset(indices));
                        }
                    } catch (const Exception& e) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0
                            && (e.getMessage() == _W("Index exceeds dimensions.")
                                || e.getMessage() == _W("Index exceeds matrix dimensions.")
                                || e.getMessage() == _W("Index exceeds variable dimensions.")
                                || e.getMessage() == _W("Invalid dimensions."))
                            && (base.isStruct() || base.isEmpty())) {
                            if (base.isStruct()) {
                                if (base.isEmpty()) {
                                    stack.push_back(base);
                                } else {
                                    ArrayOf first
                                        = ArrayOf::integerRangeConstructor(1, 1, 1, false);
                                    stack.push_back(base.getVectorSubset(first));
                                }
                            } else {
                                stack.push_back(ArrayOf::emptyConstructor());
                            }
                            break;
                        }
                        throw;
                    }
                } break;
                case OpCode::SUBIDX_BRACES: {
                    ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                    ArrayOf base = pop();
                    if (indices.empty()) {
                        Error(_W("Index expression expected."));
                    }
                    if (base.isClassType()) {
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back("{}");
                        subsindices.push_back(classIndexCell(indices));
                        bool haveFunction = false;
                        ArrayOfVector result
                            = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                        if (!haveFunction) {
                            Error(_W("Invalid indexing."));
                        }
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                        } else {
                            stack.push_back(
                                result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                        }
                        break;
                    }
                    if (ins.flags & INST_FLAG_LIST) {
                        stack.push_back(indices.size() == 1
                                ? commaListSentinel(base.getVectorContentsAsList(indices[0]))
                                : commaListSentinel(base.getNDimContentsAsList(indices)));
                        break;
                    }
                    ArrayOfVector values = indices.size() == 1
                        ? base.getVectorContentsAsList(indices[0])
                        : base.getNDimContentsAsList(indices);
                    if (values.empty()) {
                        if ((ins.flags & (INST_FLAG_PRINT | INST_FLAG_QUIET)) != 0) {
                            stack.push_back(noOutputSentinel());
                            break;
                        }
                        Error(_W("Empty expression!"));
                    }
                    stack.push_back(values[0]);
                } break;
                case OpCode::SUBIDX_DOT: {
                    ArrayOfVector params = expandCommaLists(popVector(ins.A));
                    ArrayOf base = pop();
                    const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                    if (base.isClassType()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            stringVector fields = base.getFieldNames();
                            if (std::find(fields.begin(), fields.end(), field) != fields.end()) {
                                ArrayOfVector raw = getClassdefFieldAsList(eval, base, field);
                                stack.push_back(raw.empty() ? ArrayOf::emptyConstructor() : raw[0]);
                                break;
                            }
                        }
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                        if (ins.C != 0) {
                            subtypes.push_back("()");
                            subsindices.push_back(classIndexCell(params));
                        }
                        bool haveFunction = false;
                        ArrayOfVector result
                            = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                        if (!haveFunction) {
                            if (ins.C != 0) {
                                ArrayOfVector methodResult;
                                if (eval->bytecodeInvokeObjectMethodIfExists(
                                        base, field, params, 1, methodResult)) {
                                    result = methodResult;
                                } else {
                                    result = getClassdefFieldAsList(eval, base, field);
                                }
                            } else {
                                result = getClassdefFieldAsList(eval, base, field);
                            }
                        }
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                            break;
                        }
                        stack.push_back(result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                        break;
                    }
                    if (base.isHandle() || base.isGraphicsObject()) {
                        ArrayOfVector result = getOrInvokeHandle(eval, base, field, params);
                        if (ins.flags & INST_FLAG_LIST) {
                            stack.push_back(commaListSentinel(result));
                            break;
                        }
                        stack.push_back(result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                        break;
                    }
                    if (!base.isStruct()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0 && base.isEmpty()) {
                            stack.push_back(ArrayOf::emptyConstructor());
                            break;
                        }
                        Error(_W("Invalid indexing."));
                    }
                    if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                        stringVector fields = base.getFieldNames();
                        if (std::find(fields.begin(), fields.end(), field) == fields.end()) {
                            stack.push_back(ArrayOf::emptyConstructor());
                            break;
                        }
                    }
                    ArrayOfVector fieldValues = base.getFieldAsList(field);
                    ArrayOf fieldValue
                        = fieldValues.empty() ? ArrayOf::emptyConstructor() : fieldValues[0];
                    if (ins.C != 0) {
                        if (params.empty()) {
                            Error(_W("Index expression expected."));
                        }
                        if (params.size() == 1) {
                            stack.push_back(fieldValue.getVectorSubset(params[0]));
                        } else {
                            stack.push_back(fieldValue.getNDimSubset(params));
                        }
                        break;
                    }
                    if (ins.flags & INST_FLAG_LIST) {
                        if (fieldValues.empty()) {
                            stack.push_back(ArrayOf::emptyConstructor());
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
                    std::string field;
                    try {
                        field = fieldValue.getContentAsCString();
                    } catch (const Exception&) {
                        Error(_W("Dynamic field name must be a string."));
                    }
                    if (base.isHandle() || base.isGraphicsObject()) {
                        ArrayOfVector params;
                        ArrayOfVector result = getOrInvokeHandle(eval, base, field, params);
                        stack.push_back(result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                        break;
                    }
                    if (base.isClassType()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                            stringVector fields = base.getFieldNames();
                            if (std::find(fields.begin(), fields.end(), field) != fields.end()) {
                                ArrayOfVector raw = getClassdefFieldAsList(eval, base, field);
                                stack.push_back(raw.empty() ? ArrayOf::emptyConstructor() : raw[0]);
                                break;
                            }
                        }
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                        bool haveFunction = false;
                        ArrayOfVector result
                            = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                        if (!haveFunction) {
                            result = getClassdefFieldAsList(eval, base, field);
                        }
                        stack.push_back(result.empty() ? ArrayOf::emptyConstructor() : result[0]);
                        break;
                    }
                    if (!base.isStruct()) {
                        if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0 && base.isEmpty()) {
                            stack.push_back(ArrayOf::emptyConstructor());
                            break;
                        }
                        Error(_W("Invalid indexing."));
                    }
                    if ((ins.flags & INST_FLAG_ALLOW_UNDEFINED) != 0) {
                        stringVector fields = base.getFieldNames();
                        if (std::find(fields.begin(), fields.end(), field) == fields.end()) {
                            stack.push_back(ArrayOf::emptyConstructor());
                            break;
                        }
                    }
                    ArrayOfVector fieldValues = base.getFieldAsList(field);
                    if (ins.flags & INST_FLAG_LIST) {
                        if (fieldValues.empty()) {
                            stack.push_back(ArrayOf::emptyConstructor());
                            break;
                        }
                        stack.push_back(commaListSentinel(fieldValues));
                        break;
                    }
                    stack.push_back(
                        fieldValues.empty() ? ArrayOf::emptyConstructor() : fieldValues[0]);
                } break;
                case OpCode::ASGN_PARENS: {
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
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
                            indices.reserve(2);
                            indices.push_back(rowIndex);
                            indices.push_back(colIndex);
                            indices = expandCommaLists(indices);
                        } else {
                            indices = expandCommaLists(popVector(ins.B));
                            value = pop();
                        }
                        if (indices.empty()) {
                            Error(_W("Index expression expected."));
                        }
                        ArrayOf& base = frame.locals[ins.A];
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("()");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                if (ClassdefDefinitionManager::getInstance()->loadClass(
                                        base.getClassType())) {
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
                                    if (indices.size() == 1) {
                                        base.setVectorSubset(indices[0], value);
                                    } else if (!tryAssignRowColonSlice(base, indices, value)) {
                                        base.setNDimSubset(indices, value);
                                    }
                                    markRuntimeAssigned(frame, ins.A);
                                    return;
                                }
                                Error(_W("Invalid indexing."));
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
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
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf value = pop();
                        if (indices.empty()) {
                            Error(_W("Index expression expected."));
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        ArrayOf& base = frame.locals[ins.A];
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("{}");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(_W("Invalid indexing."));
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isFunctionHandle()) {
                            Error(_W("Cannot convert base types to reference types."));
                        }
                        expandColonContentAssignmentIndices(base, indices);
                        try {
                            if (indices.size() == 1) {
                                base.setVectorContentsAsList(indices[0], values);
                            } else {
                                base.setNDimContentsAsList(indices, values);
                            }
                        } catch (const Exception& e) {
                            if (e.getMessage() == _W("Illegal zero or negative index")
                                || e.getMessage()
                                    == _W("Cannot convert string data types to indices.")) {
                                Error(
                                    _W("Not enough right hand side values to satisy left hand side "
                                       "expression."));
                            }
                            throw;
                        }
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOT: {
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf value = pop();
                        ArrayOfVector values;
                        values.push_back(value);
                        const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                        ArrayOf& base = frame.locals[ins.A];
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(_W(
                                    "Cannot apply A.field_name = B to non struct-array object A."));
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isFunctionHandle()) {
                            Error(
                                _W("Cannot apply A.field_name = B to non struct-array object A."));
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        base.setFieldAsList(field, values);
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_DOTDYN: {
                    if (ins.A >= frame.locals.size()) {
                        Error(_W("BytecodeVM: local slot out of range."));
                    }
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf fieldValue = pop();
                        ArrayOf value = pop();
                        std::string field;
                        try {
                            field = fieldValue.getContentAsCString();
                        } catch (const Exception&) {
                            Error(_W("Dynamic field name must be a string."));
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        ArrayOf& base = frame.locals[ins.A];
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(_W(
                                    "Cannot apply A.field_name = B to non struct-array object A."));
                            }
                            base = assigned;
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        if (base.isFunctionHandle()) {
                            Error(
                                _W("Cannot apply A.field_name = B to non struct-array object A."));
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            markRuntimeAssigned(frame, ins.A);
                            return;
                        }
                        base.setFieldAsList(field, values);
                        markRuntimeAssigned(frame, ins.A);
                    });
                } break;
                case OpCode::ASGN_PARENS_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        if (indices.empty()) {
                            Error(_W("Index expression expected."));
                        }
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("()");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                if (ClassdefDefinitionManager::getInstance()->loadClass(
                                        base.getClassType())) {
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
                                Error(_W("Invalid indexing."));
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isFunctionHandle()) {
                            Error(
                                _W("Cannot apply A.field_name = B to non struct-array object A."));
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
                            Error(_W("Index expression expected."));
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back("{}");
                            subsindices.push_back(classIndexCell(indices));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(_W("Invalid indexing."));
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isFunctionHandle()) {
                            Error(_W("Cannot convert base types to reference types."));
                        }
                        expandColonContentAssignmentIndices(base, indices);
                        try {
                            if (indices.size() == 1) {
                                base.setVectorContentsAsList(indices[0], values);
                            } else {
                                base.setNDimContentsAsList(indices, values);
                            }
                        } catch (const Exception& e) {
                            if (e.getMessage() == _W("Illegal zero or negative index")
                                || e.getMessage()
                                    == _W("Cannot convert string data types to indices.")) {
                                Error(
                                    _W("Not enough right hand side values to satisy left hand side "
                                       "expression."));
                            }
                            throw;
                        }
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_BRACES_MULTI_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOfVector indices = expandCommaLists(popVector(ins.B));
                        ArrayOf base = pop();
                        if (indices.empty()) {
                            Error(_W("Index expression expected."));
                        }
                        if (base.isFunctionHandle()) {
                            Error(_W("Cannot convert base types to reference types."));
                        }
                        expandColonContentAssignmentIndices(base, indices);
                        indexType targetCount = contentAssignmentTargetCount(indices);
                        indexType valueCount
                            = ins.C == 0 ? targetCount : static_cast<indexType>(ins.C);
                        if (valueCount > static_cast<indexType>(stack.size())) {
                            Error(_W("Not enough right hand side values to satisy left hand side "
                                     "expression."));
                        }
                        ArrayOfVector values(static_cast<size_t>(valueCount));
                        values.resize(static_cast<size_t>(valueCount));
                        for (int k = static_cast<int>(valueCount) - 1; k >= 0; --k) {
                            values[static_cast<size_t>(k)] = pop();
                        }
                        if (static_cast<indexType>(values.size()) < targetCount) {
                            Error(_W("Not enough right hand side values to satisy left hand side "
                                     "expression."));
                        }
                        while (static_cast<indexType>(values.size()) > targetCount) {
                            values.pop_back();
                        }
                        if (base.isEmpty() && indices.size() == 1
                            && static_cast<indexType>(values.size()) == targetCount) {
                            ArrayOfMatrix matrix(1);
                            matrix[0] = values;
                            base = ArrayOf::cellConstructor(matrix);
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
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(_W(
                                    "Cannot apply A.field_name = B to non struct-array object A."));
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            stack.push_back(base);
                            return;
                        }
                        if (base.isFunctionHandle() || (!base.isEmpty() && !base.isStruct())) {
                            Error(
                                _W("Cannot apply A.field_name = B to non struct-array object A."));
                        }
                        base.setFieldAsList(field, values);
                        stack.push_back(base);
                    });
                } break;
                case OpCode::ASGN_DOTDYN_VALUE: {
                    profiledVoidOperation(eval, SUBSASGN_OPERATOR_STR, [&]() {
                        ArrayOf fieldValue = pop();
                        ArrayOf value = pop();
                        ArrayOf base = pop();
                        std::string field;
                        try {
                            field = fieldValue.getContentAsCString();
                        } catch (const Exception&) {
                            Error(_W("Dynamic field name must be a string."));
                        }
                        ArrayOfVector values;
                        values.push_back(value);
                        if (base.isClassType()) {
                            stringVector subtypes;
                            ArrayOfVector subsindices;
                            subtypes.push_back(".");
                            subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                            bool haveFunction = false;
                            ArrayOf assigned = eval->bytecodeAssignClass(
                                base, subtypes, subsindices, value, haveFunction);
                            if (!haveFunction) {
                                Error(_W(
                                    "Cannot apply A.field_name = B to non struct-array object A."));
                            }
                            stack.push_back(assigned);
                            return;
                        }
                        if (base.isHandle() || base.isGraphicsObject()) {
                            eval->bytecodeSetHandle(base, field, values);
                            stack.push_back(base);
                            return;
                        }
                        if (base.isFunctionHandle() || (!base.isEmpty() && !base.isStruct())) {
                            Error(
                                _W("Cannot apply A.field_name = B to non struct-array object A."));
                        }
                        base.setFieldAsList(field, values);
                        stack.push_back(base);
                    });
                } break;
                case OpCode::BUILD_ROW: {
                    ArrayOfVector values = popVector(ins.A);
                    stack.push_back(eval->horzcatOperator(expandCommaLists(values)));
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
                    stack.push_back(ArrayOf::cellConstructor(matrix));
                } break;
                case OpCode::BUILD_CELL_FIELD_LIST: {
                    if (ins.B >= chunk.constants.size()) {
                        Error(_W("BytecodeVM: field constant index out of range."));
                    }
                    const auto& field = std::get<std::string>(chunk.constants.get(ins.B));
                    ArrayOf base = pop();
                    ArrayOfMatrix matrix(1);
                    if (base.isClassType()) {
                        stringVector subtypes;
                        ArrayOfVector subsindices;
                        subtypes.push_back(".");
                        subsindices.push_back(ArrayOf::characterArrayConstructor(field));
                        bool haveFunction = false;
                        ArrayOfVector result
                            = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction);
                        matrix[0]
                            = haveFunction ? result : getClassdefFieldAsList(eval, base, field);
                    } else if (base.isHandle() || base.isGraphicsObject()) {
                        ArrayOfVector params;
                        matrix[0] = getOrInvokeHandle(eval, base, field, params);
                    } else {
                        matrix[0] = base.getFieldAsList(field);
                    }
                    stack.push_back(ArrayOf::cellConstructor(matrix));
                } break;
                case OpCode::DISPLAY:
                    if (ins.A >= frame.locals.size() || ins.B >= chunk.names.size()) {
                        Error(_W("BytecodeVM: display operand out of range."));
                    }
                    eval->display(frame.locals[ins.A], chunk.names[ins.B], false, false);
                    break;
                case OpCode::DISPLAY_STACK:
                    if (stack.empty() || ins.A >= chunk.names.size()) {
                        Error(_W("BytecodeVM: display operand out of range."));
                    }
                    if (!isNoOutputSentinel(stack.back())) {
                        const std::string& displayName = stack.back().name().empty()
                            ? chunk.names[ins.A]
                            : stack.back().name();
                        eval->display(stack.back(), displayName, false, false);
                    }
                    break;
                case OpCode::DISPLAY_ANS:
                    if (stack.empty()) {
                        Error(_W("BytecodeVM: stack underflow."));
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
                    Error(_W("BytecodeVM: unhandled opcode."));
                }
            } catch (const Exception& e) {
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
        return ArrayOf::emptyConstructor();
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
                Error(_W("Wrong number of output arguments."));
            }
            size_t outputCount = static_cast<size_t>(nLhs < 0 ? 0 : nLhs);
            outputs.reserve(outputCount);
            for (size_t k = 0; k < outputCount; ++k) {
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
        if (haveVarargout && varargout.getDataClass() != NLS_CELL_ARRAY) {
            Error(_W("The special variable 'varargout' was not defined as a cell-array."));
        }

        indexType varlen = haveVarargout ? varargout.getElementCount() : 0;
        int explicitCount = static_cast<int>(chunk.retNames.size()) - 1;
        bool noArgs = (explicitCount == 0 && varlen == 0);
        if (!noArgs && !haveVarargout) {
            Error(_W("The special variable 'varargout' was not defined as expected."));
        }

        if (explicitCount == 0 && varlen > 0 && nLhs < 2) {
            outputs.resize(1);
            const ArrayOf* dp = static_cast<const ArrayOf*>(varargout.getDataPointer());
            outputs[0] = dp[0];
            outputs[0].name("");
            return outputs;
        }

        int requested = nLhs < 0 ? 0 : nLhs;
        outputs.resize(static_cast<size_t>(requested));
        int explicitToCopy = std::min(explicitCount, requested);
        for (int k = 0; k < explicitToCopy; ++k) {
            outputs[static_cast<size_t>(k)]
                = returnValueOrEmpty(eval, chunk, frame, static_cast<size_t>(k));
        }
        if (requested > explicitCount) {
            int toFill = requested - explicitCount;
            if (static_cast<indexType>(toFill) > varlen) {
                Error(_W("Not enough outputs in varargout to satisfy call."));
            }
            const ArrayOf* dp = static_cast<const ArrayOf*>(varargout.getDataPointer());
            for (int k = 0; k < toFill; ++k) {
                outputs[static_cast<size_t>(explicitCount + k)] = dp[k];
            }
        }
        return outputs;
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
                frame.locals[k] = argIn[k];
                markRuntimeAssigned(frame, k);
            }
            return;
        }

        size_t explicitCount = chunk.argNames.size() - 1;
        size_t copied = std::min(argIn.size(), explicitCount);
        copied = std::min(copied, frame.locals.size());
        for (size_t k = 0; k < copied; ++k) {
            frame.locals[k] = argIn[k];
            markRuntimeAssigned(frame, k);
        }

        if (explicitCount >= frame.locals.size()) {
            return;
        }
        size_t vargCount = argIn.size() > explicitCount ? argIn.size() - explicitCount : 0;
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(vargCount);
        auto* dp = static_cast<ArrayOf*>(varg.getReadWriteDataPointer());
        for (size_t k = 0; k < vargCount; ++k) {
            dp[k] = argIn[explicitCount + k];
        }
        varg.name("varargin");
        frame.locals[explicitCount] = varg;
        markRuntimeAssigned(frame, explicitCount);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
BytecodeVM::executeFunction(
    Evaluator* eval, const BytecodeChunk& chunk, const ArrayOfVector& argIn, int nLhs)
{
    FrameBuffer* frameBuffer = acquireFrameBuffer(chunk);
    VMCallFrame frame;
    attachFrameBuffer(frame, frameBuffer);
    frame.init(chunk, nLhs);
    bindFunctionInputsToFrame(chunk, frame, argIn);
    try {
        runChunk(eval, frame);
    } catch (...) {
        releaseFrameBuffer(frame, frameBuffer);
        throw;
    }
    syncNestedFunctionHandlesInFrame(chunk, frame);
    ArrayOfVector outputs = prepareFunctionOutputsFromFrame(eval, chunk, frame, nLhs);
    auto* macroDef = dynamic_cast<MacroFunctionDef*>(chunk.selfFunction);
    if (macroDef != nullptr && macroDef->nestedFunction) {
        syncLocalsToContext(eval, chunk, frame);
    }
    releaseFrameBuffer(frame, frameBuffer);
    return outputs;
}
//=============================================================================
void
BytecodeVM::executeScript(Evaluator* eval, const BytecodeChunk& chunk)
{
    FrameBuffer* frameBuffer = acquireFrameBuffer(chunk);
    VMCallFrame frame;
    attachFrameBuffer(frame, frameBuffer);
    frame.init(chunk, 0);
    try {
        runChunk(eval, frame);
    } catch (...) {
        syncLocalsToContext(eval, chunk, frame);
        releaseFrameBuffer(frame, frameBuffer);
        throw;
    }
    syncLocalsToContext(eval, chunk, frame);
    releaseFrameBuffer(frame, frameBuffer);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
