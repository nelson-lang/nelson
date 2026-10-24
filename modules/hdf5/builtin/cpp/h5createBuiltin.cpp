//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5createBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "h5Create.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// h5create(filename, datasetname, size)
// h5create(filename, datasetname, size, propertyName1, propertyValue1 ..., propertyNameN,
// propertyValueN)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5createBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3);
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.asWideString();
    ArrayOf param2 = argIn[1];
    std::wstring datasetname = param2.asWideString();
    ArrayOf param3 = argIn[2];
    if (!param3.isRowVector()) {
        Error("Nelson:h5create:rowVectorExpected", "Row vector expected.");
    }
    bool isSupportedSizeType = param3.dataClass() == NLS_DOUBLE && !param3.isNdArrayDoubleType();
    if (!isSupportedSizeType) {
        Error("Nelson:h5create:doubleExpected", "double expected.");
    }
    if (param3.isEmpty()) {
        Error("Nelson:h5create:rowVectorExpected", "Row vector expected.");
    }
    auto* sizePtr = (double*)param3.dataPointer();
    std::vector<double> sizeData;
    indexType nbElements = param3.elementCount();
    sizeData.reserve(nbElements);
    for (indexType i = 0; i < nbElements; i++) {
        sizeData.push_back(sizePtr[i]);
    }
    NelsonType dataType = NLS_DOUBLE;
    std::wstring textEncoding = L"system";
    std::vector<double> chunksize;
    int deflate = 0;
    ArrayOf fillvalue;
    bool fletcher32 = false;
    bool shuffle = false;
    if (argIn.size() > 3) {
        indexType nbOptions = argIn.size() - 3;
        if (nbOptions % 2 != 0) {
            Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
        }
    }
    for (size_t i = 3; i + 1 < argIn.size(); i += 2) {
        ArrayOf paramXname = argIn[i];
        ArrayOf paramXvalue = argIn[i + 1];
        if (paramXname.asWideString() == L"Datatype") {
            std::wstring dataTypeStr = paramXvalue.asWideString();
            if (dataTypeStr == L"double") {
                dataType = NLS_DOUBLE;
            } else if (dataTypeStr == L"single") {
                dataType = NLS_SINGLE;
            } else if (dataTypeStr == L"int8") {
                dataType = NLS_INT8;
            } else if (dataTypeStr == L"uint8") {
                dataType = NLS_UINT8;
            } else if (dataTypeStr == L"int16") {
                dataType = NLS_INT16;
            } else if (dataTypeStr == L"uint16") {
                dataType = NLS_UINT16;
            } else if (dataTypeStr == L"int32") {
                dataType = NLS_INT32;
            } else if (dataTypeStr == L"uint32") {
                dataType = NLS_UINT32;
            } else if (dataTypeStr == L"int64") {
                dataType = NLS_INT64;
            } else if (dataTypeStr == L"uint64") {
                dataType = NLS_UINT64;
            } else {
                Error(L"Nelson:h5create:typeNotSupported", L"Type not supported.");
            }
        } else if (paramXname.asWideString() == L"ChunkSize") {
            bool isSupportedSizeType
                = paramXvalue.dataClass() == NLS_DOUBLE && !paramXvalue.isNdArrayDoubleType();
            if (!isSupportedSizeType) {
                Error("Nelson:h5create:doubleExpected", "double expected.");
            }
            if (paramXvalue.isEmpty()) {
                Error("Nelson:h5create:rowVectorExpected", "Row vector expected.");
            }
            double* chunkSizePtr = (double*)paramXvalue.dataPointer();
            indexType nbElements = paramXvalue.elementCount();
            chunksize.reserve(nbElements);
            for (indexType i = 0; i < nbElements; i++) {
                chunksize.push_back(chunkSizePtr[i]);
            }
        } else if (paramXname.asWideString() == L"Deflate") {
            deflate = (int)paramXvalue.asScalarIndex(true);
        } else if (paramXname.asWideString() == L"FillValue") {
            if (!paramXvalue.isScalar()) {
                Error(L"Nelson:validators:mustBeScalar", L"%s must be a scalar.", L"Value");
            }
            NelsonType valueClass = paramXvalue.dataClass();
            bool isSupportedType = (valueClass == NLS_DOUBLE || valueClass == NLS_SINGLE
                || IS_INTEGER_TYPE(valueClass));
            if (!isSupportedType) {
                Error(L"Nelson:h5create:unsupportedValueType", L"Unsupported value type.");
            }
            fillvalue = paramXvalue;
        } else if (paramXname.asWideString() == L"Fletcher32") {
            fletcher32 = paramXvalue.asLogicalScalar();
        } else if (paramXname.asWideString() == L"Shuffle") {
            shuffle = paramXvalue.asLogicalScalar();
        } else if (paramXname.asWideString() == L"TextEncoding") {
            std::wstring _textEncoding = paramXvalue.asWideString();
            if (_textEncoding == L"system" || _textEncoding == L"UTF-8") {
                textEncoding.assign(_textEncoding);
            } else {
                Error(L"Nelson:h5create:invalidTextEncoding", L"Wrong text encoding parameter.");
            }
        } else {
            Error(L"Nelson:h5create:invalidParameter", L"Invalid parameter.");
        }
    }
    h5Create(filename, datasetname, sizeData, dataType, chunksize, deflate, fillvalue, fletcher32,
        shuffle, textEncoding);
    return retval;
}
//=============================================================================
