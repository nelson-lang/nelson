//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <algorithm>
#include <cstring>
#include "Fscanf_helpers.hpp"
#include "FscanFunction.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
convertToArrayOfFscanf(ArrayOfVector& values, NelsonType classDestination, bool haveThirdArgument,
    double m, double n, bool mixed)
{
    ArrayOf value;
    indexType nbValues;
    Dimensions nDims;
    Dimensions dims;

    if (mixed) {
        ArrayOf v = values[0];
        Dimensions dimsV = v.getDimensions();
        if (haveThirdArgument) {
            if (n == -1) {
                nbValues = std::min(dimsV.getElementCount(), (indexType)m);
                nDims = Dimensions(nbValues, 1);
            } else {
                nbValues = dimsV.getElementCount();
                if (std::isinf(n)) {
                    n = nbValues / m;
                }
                nDims = Dimensions((indexType)m, (indexType)n);
            }
        } else {
            nbValues = dimsV.getElementCount();
            nDims = Dimensions(nbValues, 1);
        }
        dims = Dimensions(nbValues, 1);
        T* ptr = (T*)ArrayOf::allocateArrayOf(classDestination, nbValues, stringVector(), false);
        value = ArrayOf(classDestination, dims, ptr);
        T* src = (T*)v.getDataPointer();
        std::memcpy(ptr, src, sizeof(T) * dims.getElementCount());
    } else {
        if (haveThirdArgument) {
            if (n == -1) {
                nbValues = std::min(values.size(), (size_t)m);
                nDims = Dimensions(nbValues, 1);
            } else {
                nbValues = values.size();
                if (std::isinf(n)) {
                    n = nbValues / m;
                }
                nDims = Dimensions((indexType)m, (indexType)n);
            }
        } else {
            nbValues = values.size();
            nDims = Dimensions(nbValues, 1);
        }
        dims = Dimensions(nbValues, 1);
        T* ptr = (T*)ArrayOf::allocateArrayOf(classDestination, nbValues, stringVector(), false);
        value = ArrayOf(classDestination, dims, ptr);
        switch (classDestination) {
        case NLS_UINT64: {
            for (indexType k = 0; k < nbValues; k++) {
                ptr[k] = (T)values[k].getContentAsUnsignedInteger64Scalar();
            }
        } break;
        case NLS_INT64: {
            for (indexType k = 0; k < nbValues; k++) {
                ptr[k] = (T)values[k].getContentAsInteger64Scalar();
            }
        } break;
        case NLS_DOUBLE:
        default: {
            for (indexType k = 0; k < nbValues; k++) {
                ptr[k] = (T)values[k].getContentAsDoubleScalar();
            }
        } break;
        }
    }
    if (dims.getElementCount() > nDims.getElementCount()) {
        value.resize(nDims);
    } else if (!dims.equals(nDims) && dims.getElementCount() == nDims.getElementCount()) {
        value.reshape(nDims);
    }
    return value;
}
//=============================================================================
ArrayOf
FscanF(FILE* filepointer, const std::string& format, const std::string& encoding, double m,
    double n, bool haveThirdArgument, indexType& count, bool asSscanf)
{
    enum OutputType outType = AS_NONE;
    ArrayOfVector values;
    bool bContinue = true;
    int resf = 0;
    bool noSize = (m == -1) && (n == -1);

    while (bContinue) {
        if ((values.size() >= (m * n)) && !noSize) {
            bContinue = false;
            break;
        }
        if (feof(filepointer)) {
            bContinue = false;
            break;
        }
        char* buff = new char[format.size() + 1];
        strcpy(buff, format.c_str());
        char* dp = buff;
        char* np;
        char sv;
        while (*dp) {
            np = dp;
            while ((*dp) && (*dp != '%')) {
                dp++;
            }
            sv = *dp;
            *dp = 0;
            resf = fscanf(filepointer, np, NULL);
            if (feof(filepointer)) {
                bContinue = false;
            }
            *dp = sv;
            if (*dp) {
                np = validateScanFormatSpec(dp + 1);
                if (!np) {
                    if (buff) {
                        delete[] buff;
                        buff = nullptr;
                    }
                    Error(_W("Invalid format."));
                } else {
                    if (*(np - 1) == '%') {
                        resf = fscanf(filepointer, "%%");
                        if (resf == EOF) {
                            bContinue = false;
                        }
                        dp += 2;
                    } else {
                        bool as64bit = false;
                        if (*(np - 1) == 'l') {
                            as64bit = true;
                            np++;
                        }
                        sv = *np;
                        *np = 0;
                        switch (*(np - 1)) {
                        case 'd':
                        case 'i': {
                            if (as64bit) {
                                long sdumint64;
                                resf = fscanf(filepointer, dp, &sdumint64);
                                if (feof(filepointer) || resf == EOF || resf == 0) {
                                    bContinue = false;
                                    break;
                                }
                                if (outType == AS_NONE) {
                                    outType = AS_INT64;
                                } else {
                                    if (outType != AS_INT64) {
                                        outType = AS_MIXED;
                                    }
                                }
                                values.push_back(ArrayOf::int64Constructor((int64)sdumint64));

                            } else {
                                int sdumdouble;
                                resf = fscanf(filepointer, dp, &sdumdouble);
                                if (resf == EOF || resf == 0) {
                                    bContinue = false;
                                    break;
                                }
                                if (outType == AS_NONE) {
                                    outType = AS_DOUBLE;
                                } else {
                                    if (outType != AS_DOUBLE) {
                                        outType = AS_MIXED;
                                    }
                                }
                                values.push_back(ArrayOf::doubleConstructor((double)sdumdouble));
                            }
                        } break;
                        case 'o':
                        case 'u':
                        case 'x':
                        case 'X': {
                            if (as64bit) {
                                unsigned long sdumint64;
                                resf = fscanf(filepointer, dp, &sdumint64);
                                if (resf == EOF || resf == 0) {
                                    bContinue = false;
                                    break;
                                }
                                if (outType == AS_NONE) {
                                    outType = AS_UINT64;
                                } else {
                                    if (outType != AS_UINT64) {
                                        outType = AS_MIXED;
                                    }
                                }
                                values.push_back(ArrayOf::uint64Constructor((uint64)sdumint64));

                            } else {
                                unsigned int dumpUnsignedInt;
                                resf = fscanf(filepointer, dp, &dumpUnsignedInt);
                                if (resf == EOF || resf == 0) {
                                    bContinue = false;
                                    break;
                                }
                                if (outType == AS_NONE) {
                                    outType = AS_DOUBLE;
                                } else {
                                    if (outType != AS_DOUBLE) {
                                        outType = AS_MIXED;
                                    }
                                }
                                values.push_back(
                                    ArrayOf::doubleConstructor((double)dumpUnsignedInt));
                            }
                        } break;
                        case 'e':
                        case 'E':
                        case 'f':
                        case 'F':
                        case 'g':
                        case 'G': {
                            single dumpAsFloat;
                            resf = fscanf(filepointer, dp, &dumpAsFloat);
                            if (resf == EOF || resf == 0)
                                bContinue = false;
                            else {
                                if (outType == AS_NONE) {
                                    outType = AS_DOUBLE;
                                } else {
                                    if (outType != AS_DOUBLE) {
                                        outType = AS_MIXED;
                                    }
                                }
                                values.push_back(ArrayOf::doubleConstructor(dumpAsFloat));
                            }
                        } break;
                        case 'c':
                        case 's': {
                            if (outType == AS_NONE) {
                                outType = AS_STRING;
                            } else {
                                if (outType != AS_STRING) {
                                    outType = AS_MIXED;
                                }
                            }
                            char buffer[8192];
                            resf = fscanf(filepointer, dp, buffer);
                            if (resf == EOF || resf == 0) {
                                bContinue = false;
                                break;
                            }
                            if (*(np - 1) == 'c') {
                                buffer[resf] = 0;
                            }
                            std::string charset(buffer);
                            std::string asUtf8;
                            charsetToUtf8Converter(charset, encoding, asUtf8);
                            values.push_back(ArrayOf::characterArrayConstructor(asUtf8));
                        } break;
                        default: {
                            if (buff) {
                                delete[] buff;
                                buff = nullptr;
                            }
                            Error(_W("Unsupported fscanf format."));
                        } break;
                        }
                        *np = sv;
                        dp = np;
                    }
                }
            }
        }
        if (buff) {
            delete[] buff;
            buff = nullptr;
        }
    }
    count = values.size();
    ArrayOf value;
    if (count == 0) {
        Dimensions empty(0, 0);
        value = ArrayOf::emptyConstructor(empty);
        switch (outType) {
        case AS_STRING: {
            value.promoteType(NLS_CHAR);
        } break;
        case AS_INT64: {
            value.promoteType(NLS_INT64);
        } break;
        case AS_UINT64: {
            value.promoteType(NLS_UINT64);
        } break;
        case AS_DOUBLE:
        case AS_MIXED:
        case AS_NONE:
        default: {
            value.promoteType(NLS_DOUBLE);
        } break;
        }
        return value;
    }
    switch (outType) {
    case AS_STRING: {
        std::wstring strs;
        for (auto& value : values) {
            std::wstring str = value.getContentAsWideString();
            strs.append(str);
        }
        value = ArrayOf::characterArrayConstructor(strs);
    } break;
    case AS_DOUBLE: {
        value = convertToArrayOfFscanf<double>(values, NLS_DOUBLE, haveThirdArgument, m, n, false);
    } break;
    case AS_INT64: {
        value = convertToArrayOfFscanf<int64>(values, NLS_INT64, haveThirdArgument, m, n, false);
    } break;
    case AS_UINT64: {
        value = convertToArrayOfFscanf<uint64>(values, NLS_UINT64, haveThirdArgument, m, n, false);
    } break;
    case AS_MIXED: {
        std::vector<double> v;
        v.reserve(values.size());
        for (ArrayOf value : values) {
            switch (value.getDataClass()) {
            case NLS_UINT64:
            case NLS_INT64:
            case NLS_DOUBLE: {
                v.push_back(value.getContentAsDoubleScalar());
            } break;
            case NLS_CHAR: {
                value.promoteType(NLS_DOUBLE);
                Dimensions dimsValue = value.getDimensions();
                v.reserve(v.size() + dimsValue.getElementCount());
                double* ptr = (double*)value.getDataPointer();
                indexType elementCount = dimsValue.getElementCount();
                for (indexType k = 0; k < elementCount; k++) {
                    v.push_back(ptr[k]);
                }
            } break;
            default: {
            } break;
            }
        }
        ArrayOfVector promoted;
        double* ptr
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, v.size(), stringVector(), false);
        Dimensions dims(v.size(), 1);
        value = ArrayOf(NLS_DOUBLE, dims, ptr);
        std::memcpy(ptr, v.data(), sizeof(double) * v.size());
        promoted.push_back(value);
        value = convertToArrayOfFscanf<double>(promoted, NLS_DOUBLE, haveThirdArgument, m, n, true);
    } break;
    case AS_NONE: {
    } break;
    default: {
    } break;
    }
    return value;
}
//=============================================================================
}
//=============================================================================
