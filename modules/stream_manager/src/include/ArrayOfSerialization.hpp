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
#include "nlsStream_manager_exports.h"
#include "ArrayOf.hpp"
#include "BinaryStream.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// A function_handle stores its definition behind a raw pointer, which is
// meaningless in another process. To serialize one, a consumer registers an
// encoder that turns it into a plain class array tagged with this marker, and a
// decoder that rebuilds it. The walker applies them recursively so handles
// nested inside cells/structs are handled too.
#define NLS_FUNCTION_HANDLE_SERIALIZATION_MARKER "__nls_function_handle__"
//=============================================================================
using FunctionHandleSerializationEncoder = ArrayOf (*)(const ArrayOf&);
using FunctionHandleSerializationDecoder = ArrayOf (*)(const ArrayOf&, bool&);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP void
registerFunctionHandleSerializers(
    FunctionHandleSerializationEncoder encoder, FunctionHandleSerializationDecoder decoder);
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP ArrayOfSerialization
{
public:
    //=============================================================================
    ArrayOfSerialization();
    //=============================================================================
    ArrayOfSerialization(const ArrayOf& data);
    //=============================================================================
    void
    clear();
    //=============================================================================
    bool
    set(const ArrayOf& data);
    //=============================================================================
    ArrayOf
    get(bool& success);
    //=============================================================================
    bool
    isFullySerialized();
    //=============================================================================
    void
    write(BinaryWriter& writer) const;
    //=============================================================================
    bool
    read(BinaryReader& reader);
    //=============================================================================
private:
    bool fullySerialized;
    int nelsonObjectClass;
    bool isSparse;
    std::string classTypeName;
    std::vector<indexType> dims;
    std::vector<std::string> fieldnames;
    std::vector<int8> asInt8;
    std::vector<uint8> asUint8;
    std::vector<int16> asInt16;
    std::vector<uint16> asUint16;
    std::vector<int32> asInt32;
    std::vector<uint32> asUint32;
    std::vector<int64> asInt64;
    std::vector<uint64> asUint64;
    std::vector<double> asDouble;
    std::vector<single> asSingle;
    std::vector<charType> asCharacter;
    uint64 nzmax;
    std::vector<uint64> I;
    std::vector<uint64> J;
    std::vector<ArrayOfSerialization> subArrayOf;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
