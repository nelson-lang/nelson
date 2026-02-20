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
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "h5Create.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// h5create(filename, datasetname, size)
// h5create(filename, datasetname, size, propretyName1, propertyValue1 ..., propretyNameN,
// propertyValueN)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5createBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3);
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    ArrayOf param2 = argIn[1];
    std::wstring datasetname = param2.getContentAsWideString();
    ArrayOf param3 = argIn[2];
    if (!param3.isRowVector()) {
        raiseError2(_E("nelson:validators:mustBeRowVectorAtPosition"), 3);
    }
    bool isSupportedSizeType = param3.getDataClass() == NLS_DOUBLE && !param3.isNdArrayDoubleType();
    if (!isSupportedSizeType) {
        raiseError2(_E("nelson:validators:mustBeDoubleAtPosition"), 3);
    }
    if (param3.isEmpty()) {
        raiseError2(_E("nelson:validators:mustBeRowVectorAtPosition"), 3);
    }
    auto* sizePtr = (double*)param3.getDataPointer();
    std::vector<double> sizeData;
    indexType nbElements = param3.getElementCount();
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
            raiseError2(_E("nelson:arguments:wrongNumberOfInputs"));
        }
    }
    for (size_t i = 3; i + 1 < argIn.size(); i += 2) {
        ArrayOf paramXname = argIn[i];
        ArrayOf paramXvalue = argIn[i + 1];
        if (paramXname.getContentAsWideString() == L"Datatype") {
            std::wstring dataTypeStr = paramXvalue.getContentAsWideString();
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
                raiseError2(_E("nelson:internal:typeNotManaged"));
            }
        } else if (paramXname.getContentAsWideString() == L"ChunkSize") {
            bool isSupportedSizeType
                = paramXvalue.getDataClass() == NLS_DOUBLE && !paramXvalue.isNdArrayDoubleType();
            if (!isSupportedSizeType) {
                raiseError2(_E("nelson:validators:mustBeDoubleAtPosition"), i + 1);
            }
            if (paramXvalue.isEmpty()) {
                raiseError2(_E("nelson:validators:mustBeRowVectorAtPosition"), i + 1);
            }
            double* chunkSizePtr = (double*)paramXvalue.getDataPointer();
            indexType nbElements = paramXvalue.getElementCount();
            chunksize.reserve(nbElements);
            for (indexType i = 0; i < nbElements; i++) {
                chunksize.push_back(chunkSizePtr[i]);
            }
        } else if (paramXname.getContentAsWideString() == L"Deflate") {
            deflate = (int)paramXvalue.getContentAsScalarIndex(true);
        } else if (paramXname.getContentAsWideString() == L"FillValue") {
            if (!paramXvalue.isScalar()) {
                raiseError2(_E("nelson:validators:mustBeScalar"));
            }
            NelsonType valueClass = paramXvalue.getDataClass();
            bool isSupportedType = (valueClass == NLS_DOUBLE || valueClass == NLS_SINGLE
                || IS_INTEGER_TYPE(valueClass));
            if (!isSupportedType) {
                raiseError2(_E("nelson:validators:mustBeIntegerFormAtPosition"), i + 1);
            }
            fillvalue = paramXvalue;
        } else if (paramXname.getContentAsWideString() == L"Fletcher32") {
            fletcher32 = paramXvalue.getContentAsLogicalScalar();
        } else if (paramXname.getContentAsWideString() == L"Shuffle") {
            shuffle = paramXvalue.getContentAsLogicalScalar();
        } else if (paramXname.getContentAsWideString() == L"TextEncoding") {
            std::wstring _textEncoding = paramXvalue.getContentAsWideString();
            if (_textEncoding == L"system" || _textEncoding == L"UTF-8") {
                textEncoding.assign(_textEncoding);
            } else {
                raiseError(L"Nelson:hdf5:ERROR_WRONG_TEXT_ENCODING_PARAMETER",
                    ERROR_WRONG_TEXT_ENCODING_PARAMETER);
            }
        } else {
            raiseError(
                L"Nelson:hdf5:ERROR_INVALID_NELSON_PARAMETER", ERROR_INVALID_NELSON_PARAMETER);
        }
    }
    h5Create(filename, datasetname, sizeData, dataType, chunksize, deflate, fillvalue, fletcher32,
        shuffle, textEncoding);
    return retval;
}
//=============================================================================
