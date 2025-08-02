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
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>
#include "nlsStream_manager_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
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
private:
    bool fullySerialized;
    int nelsonObjectClass;
    bool isSparse;
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
    //=============================================================================
    friend class boost::serialization::access;
    //=============================================================================
    template <class Archive>
    void
    serialize(Archive& ar, const unsigned int version)
    {
        ar& fullySerialized;
        ar& nelsonObjectClass;
        ar& isSparse;
        ar& dims;
        ar& fieldnames;
        if (isSparse) {
            ar& I;
            ar& J;
            ar& nzmax;
        }
        switch ((NelsonType)nelsonObjectClass) {
        case NLS_UNKNOWN:
        case NLS_GO_HANDLE:
        case NLS_HANDLE:
        default: {
        } break;
        case NLS_CELL_ARRAY:
        case NLS_STRUCT_ARRAY:
        case NLS_STRING_ARRAY: {
            ar& subArrayOf;
        } break;
        case NLS_LOGICAL:
        case NLS_UINT8: {
            ar& asUint8;
        } break;
        case NLS_INT8: {
            ar& asInt8;
        } break;
        case NLS_UINT16: {
            ar& asUint16;
        } break;
        case NLS_INT16: {
            ar& asInt16;
        } break;
        case NLS_UINT32: {
            ar& asUint32;
        } break;
        case NLS_INT32: {
            ar& asInt32;
        } break;
        case NLS_UINT64: {
            ar& asUint64;
        } break;
        case NLS_INT64: {
            ar& asInt64;
        } break;
        case NLS_SCOMPLEX:
        case NLS_SINGLE: {
            ar& asSingle;
        } break;
        case NLS_DCOMPLEX:
        case NLS_DOUBLE: {
            ar& asDouble;
        } break;
        case NLS_CHAR: {
            ar& asCharacter;
        } break;
        }
    }
};
//=============================================================================
}
//=============================================================================
