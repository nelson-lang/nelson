//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CompressedStringHelpers.hpp"
#include "ArrayOfSerialization.hpp"
#include "BinaryStream.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefSerialization.hpp"
#include "NelsonConfiguration.hpp"
#include "StringZLib.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Wire header for a serialized ArrayOf payload. Bump the version on any layout
// change so that a mismatched build rejects the payload instead of misreading
// it. This format is process-to-process on the same machine only, never on disk.
static constexpr uint32_t ARRAYOF_WIRE_MAGIC = 0x414E4C53u; // 'S','L','N','A'
static constexpr uint16_t ARRAYOF_WIRE_VERSION = 1;
//=============================================================================
static int
markerFieldIndex(const stringVector& fieldnames, const std::string& name);
static ArrayOf
encodeFunctionHandleForSerialization(const ArrayOf& value);
static ArrayOf
decodeFunctionHandleFromSerialization(const ArrayOf& marker, bool& success);
static void
ensureFunctionHandleSerializersRegistered();
static void
restoreClassdefLayouts(Evaluator* eval, ArrayOf& data);
static void
applyClassdefSaveHooks(Evaluator* eval, ArrayOf& data);
//=============================================================================
static Evaluator*
ipcMainEvaluator()
{
    return (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
}
//=============================================================================
std::string
ArrayOfToCompressedString(const ArrayOf& data, bool& fullySerialized)
{
    ensureFunctionHandleSerializersRegistered();
    // Replace any classdef that defines saveObjectImpl (e.g. dictionary, whose entries
    // live in a per-process native store that cannot cross a process boundary) with its
    // saved contents before serializing; the receiver rebuilds it via loadObjectImpl.
    ArrayOf prepared = data;
    applyClassdefSaveHooks(ipcMainEvaluator(), prepared);
    ArrayOfSerialization serializedVariable(prepared);
    fullySerialized = serializedVariable.isFullySerialized();
    std::string serialized;
    BinaryWriter writer(serialized);
    writer.writePod<uint32_t>(ARRAYOF_WIRE_MAGIC);
    writer.writePod<uint16_t>(ARRAYOF_WIRE_VERSION);
    serializedVariable.write(writer);
    bool failed = false;
    std::string serialized_compressed_string = compressString(serialized, failed);
    if (failed) {
        fullySerialized = false;
        return {};
    }
    return serialized_compressed_string;
}
//=============================================================================
ArrayOf
CompressedStringToArrayOf(const std::string& compressedString, bool& success)
{
    ArrayOf res;
    success = false;
    ensureFunctionHandleSerializersRegistered();
    bool failed = false;
    std::string decompressedVariable = decompressString(compressedString, failed);
    if (!failed) {
        BinaryReader reader(decompressedVariable);
        uint32_t magic = 0;
        uint16_t version = 0;
        if (!reader.readPod(magic) || !reader.readPod(version) || magic != ARRAYOF_WIRE_MAGIC
            || version != ARRAYOF_WIRE_VERSION) {
            success = false;
            return res;
        }
        ArrayOfSerialization serializedVariable;
        if (serializedVariable.read(reader)) {
            res = serializedVariable.get(success);
            if (success) {
                restoreClassdefLayouts(ipcMainEvaluator(), res);
            }
            if (!success && !serializedVariable.isFullySerialized()) {
                success = true;
            }
        } else {
            success = false;
        }
    }
    return res;
}
//=============================================================================
static int
markerFieldIndex(const stringVector& fieldnames, const std::string& name)
{
    for (int k = 0; k < static_cast<int>(fieldnames.size()); ++k) {
        if (fieldnames[k] == name) {
            return k;
        }
    }
    return -1;
}
//=============================================================================
// A function_handle keeps its definition behind a raw pointer that is
// meaningless in another process. Encode it into a plain class array holding
// the anonymous source, whether it is a named handle, its arguments, captured
// variable names and captured values; decode rebuilds an AnonymousMacroFunctionDef
// (whose constructor defers parsing, so it is safe off the main thread).
static ArrayOf
encodeFunctionHandleForSerialization(const ArrayOf& value)
{
    function_handle fh = value.asFunctionHandle();
    auto* def = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
    std::string content = (def != nullptr) ? def->getContent() : std::string();
    bool isNamed = (def != nullptr) ? def->isFunctionHandle() : true;
    stringVector arguments;
    stringVector names;
    std::vector<ArrayOf> variables;
    if (def != nullptr && !isNamed) {
        arguments = def->getArguments();
        names = def->getVariableNames();
        variables = def->getVariables();
    }
    stringVector fieldnames = { "content", "is_named", "arguments", "variable_names", "variables" };
    auto* elements
        = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CLASS_ARRAY, 1, fieldnames, false));
    elements[0] = ArrayOf::characterArray(content);
    elements[1] = ArrayOf::logicalScalar(isNamed);
    elements[2] = ArrayOf::stringArray(arguments, Dimensions(1, arguments.size()));
    elements[3] = ArrayOf::stringArray(names, Dimensions(1, names.size()));
    auto* cell = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, variables.size(), stringVector(), false));
    for (size_t k = 0; k < variables.size(); ++k) {
        cell[k] = variables[k];
    }
    elements[4] = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, variables.size()), cell);
    ArrayOf marker(NLS_CLASS_ARRAY, Dimensions(1, 1), elements, false, fieldnames);
    marker.setClassType(NLS_FUNCTION_HANDLE_SERIALIZATION_MARKER);
    return marker;
}
//=============================================================================
static ArrayOf
decodeFunctionHandleFromSerialization(const ArrayOf& marker, bool& success)
{
    success = false;
    if (!marker.isClassType()
        || marker.classType() != std::string(NLS_FUNCTION_HANDLE_SERIALIZATION_MARKER)) {
        return marker;
    }
    const auto* elements = static_cast<const ArrayOf*>(marker.dataPointer());
    if (elements == nullptr) {
        return marker;
    }
    const stringVector fieldnames = marker.fieldNames();
    const int contentIndex = markerFieldIndex(fieldnames, "content");
    const int isNamedIndex = markerFieldIndex(fieldnames, "is_named");
    const int argumentsIndex = markerFieldIndex(fieldnames, "arguments");
    const int namesIndex = markerFieldIndex(fieldnames, "variable_names");
    const int variablesIndex = markerFieldIndex(fieldnames, "variables");
    if (contentIndex < 0 || isNamedIndex < 0 || argumentsIndex < 0 || namesIndex < 0
        || variablesIndex < 0) {
        return marker;
    }
    std::string content = elements[contentIndex].asUtf8String();
    bool isNamed = elements[isNamedIndex].asLogicalScalar();
    function_handle fh;
    fh.anonymousHandle = nullptr;
    if (isNamed) {
        fh.anonymousHandle
            = reinterpret_cast<nelson_handle*>(new AnonymousMacroFunctionDef(content));
    } else {
        stringVector arguments = elements[argumentsIndex].asUtf8StringVector(false);
        stringVector names = elements[namesIndex].asUtf8StringVector(false);
        ArrayOf variablesCell = elements[variablesIndex];
        std::vector<ArrayOf> variables;
        indexType variableCount = variablesCell.elementCount();
        variables.reserve(variableCount);
        const auto* cell = static_cast<const ArrayOf*>(variablesCell.dataPointer());
        for (indexType k = 0; cell != nullptr && k < variableCount; ++k) {
            variables.push_back(cell[k]);
        }
        fh.anonymousHandle = reinterpret_cast<nelson_handle*>(
            new AnonymousMacroFunctionDef(content, arguments, names, variables));
    }
    ArrayOf result = ArrayOf::functionHandle(fh);
    success = true;
    return result;
}
//=============================================================================
static void
ensureFunctionHandleSerializersRegistered()
{
    static bool registered = false;
    if (!registered) {
        registerFunctionHandleSerializers(
            encodeFunctionHandleForSerialization, decodeFunctionHandleFromSerialization);
        registered = true;
    }
}
//=============================================================================
static void
restoreClassdefLayouts(Evaluator* eval, ArrayOf& data)
{
    switch (data.dataClass()) {
    case NLS_CLASSDEF_ARRAY: {
        const std::string className = data.classType();
        classdefLoadClass(className);
        // restore nested objects first: a stored value may itself be a hooked object
        auto* fields = (ArrayOf*)data.dataPointer();
        indexType fieldTotal = data.elementCount() * (indexType)data.fieldNames().size();
        for (indexType k = 0; k < fieldTotal && fields != nullptr; ++k) {
            restoreClassdefLayouts(eval, fields[k]);
        }
        ArrayOf restored;
        if (eval != nullptr && classdefRestoreLoadObject(eval, className, data, restored)) {
            // the class defines loadObjectImpl: rebuild it from its saved contents
            data = restored;
        } else {
            data.setClassdefLayout(classdefLayout(className));
        }
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE: {
        auto* elements = (ArrayOf*)data.dataPointer();
        indexType elementCount = data.elementCount();
        for (indexType k = 0; k < elementCount && elements != nullptr; ++k) {
            restoreClassdefLayouts(eval, elements[k]);
        }
    } break;
    default: {
    } break;
    }
}
//=============================================================================
static void
applyClassdefSaveHooks(Evaluator* eval, ArrayOf& data)
{
    switch (data.dataClass()) {
    case NLS_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        if (eval != nullptr) {
            ArrayOf saved;
            if (classdefPrepareSaveObject(eval, data, saved)) {
                data = saved;
            }
        }
        if (data.dataClass() == NLS_CLASSDEF_ARRAY || data.dataClass() == NLS_CLASS_ARRAY) {
            auto* fields = (ArrayOf*)data.dataPointer();
            indexType fieldTotal = data.elementCount() * (indexType)data.fieldNames().size();
            for (indexType k = 0; k < fieldTotal && fields != nullptr; ++k) {
                applyClassdefSaveHooks(eval, fields[k]);
            }
        }
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_FUNCTION_HANDLE: {
        auto* elements = (ArrayOf*)data.dataPointer();
        indexType elementCount = data.elementCount();
        for (indexType k = 0; k < elementCount && elements != nullptr; ++k) {
            applyClassdefSaveHooks(eval, elements[k]);
        }
    } break;
    default: {
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
