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
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstring>
#include "Fscanf_helpers.hpp"
#include "FscanFunction.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
        Dimensions dimsV = v.dimensions();
        if (haveThirdArgument) {
            if (n == -1) {
                nbValues = std::min(dimsV.elementCount(), (indexType)m);
                nDims = Dimensions(nbValues, 1);
            } else {
                nbValues = dimsV.elementCount();
                if (std::isinf(n)) {
                    n = nbValues / m;
                }
                nDims = Dimensions((indexType)m, (indexType)n);
            }
        } else {
            nbValues = dimsV.elementCount();
            nDims = Dimensions(nbValues, 1);
        }
        dims = Dimensions(nbValues, 1);
        T* ptr = (T*)ArrayOf::allocateArrayOf(classDestination, nbValues, stringVector(), false);
        value = ArrayOf(classDestination, dims, ptr);
        T* src = (T*)v.dataPointer();
        std::memcpy(ptr, src, sizeof(T) * dims.elementCount());
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
                ptr[k] = (T)values[k].asUint64Scalar();
            }
        } break;
        case NLS_INT64: {
            for (indexType k = 0; k < nbValues; k++) {
                ptr[k] = (T)values[k].asInt64Scalar();
            }
        } break;
        case NLS_DOUBLE:
        default: {
            for (indexType k = 0; k < nbValues; k++) {
                ptr[k] = (T)values[k].asDoubleScalar();
            }
        } break;
        }
    }
    if (dims.elementCount() > nDims.elementCount()) {
        value.resize(nDims);
    } else if (!dims.equals(nDims) && dims.elementCount() == nDims.elementCount()) {
        value.reshape(nDims);
    }
    return value;
}
//=============================================================================
// Matches the literal text between two conversions like fscanf does: a blank
// matches any run of whitespace, any other character must match the stream.
// Returns EOF when the stream ends before a character could match, else 0.
static int
matchLiteralText(FILE* filepointer, const char* text)
{
    for (const char* p = text; *p; ++p) {
        int c;
        if (isspace(static_cast<unsigned char>(*p))) {
            do {
                c = fgetc(filepointer);
            } while (c != EOF && isspace(c));
            if (c != EOF) {
                ungetc(c, filepointer);
            }
            continue;
        }
        c = fgetc(filepointer);
        if (c == EOF) {
            return EOF;
        }
        if (c != static_cast<unsigned char>(*p)) {
            ungetc(c, filepointer);
            return 0;
        }
    }
    return feof(filepointer) ? EOF : 0;
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
        size_t valuesBefore = values.size();
        long passStart = ftell(filepointer);
        while (*dp) {
            np = dp;
            while ((*dp) && (*dp != '%')) {
                dp++;
            }
            sv = *dp;
            *dp = 0;
            if (*np != 0) {
                resf = matchLiteralText(filepointer, np);
                if (resf == EOF || feof(filepointer)) {
                    bContinue = false;
                }
            }
            *dp = sv;
            if (*dp) {
                np = validateScanFormatSpec(dp + 1);
                if (!np) {
                    if (buff) {
                        delete[] buff;
                        buff = nullptr;
                    }
                    Error(L"Nelson:stream_manager:invalidFormat", L"Invalid format.");
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
                        if (*(dp + 1) == '*') {
                            // Assignment suppression ("%*d"): advance the input, store nothing.
                            bool bOK = scanfConsumeSuppressed(filepointer, std::string(dp));
                            if (!bOK) {
                                bContinue = false;
                            }
                            *np = sv;
                            dp = np;
                            continue;
                        }
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
                                values.push_back(ArrayOf::int64Scalar((int64)sdumint64));

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
                                values.push_back(ArrayOf::doubleScalar((double)sdumdouble));
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
                                values.push_back(ArrayOf::uint64Scalar((uint64)sdumint64));

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
                                values.push_back(ArrayOf::doubleScalar((double)dumpUnsignedInt));
                            }
                        } break;
                        case 'e':
                        case 'E':
                        case 'f':
                        case 'F':
                        case 'g':
                        case 'G': {
                            // Read as double to keep full precision: a plain spec such
                            // as "%f" gets the 'l' length modifier.
                            std::string spec(dp);
                            if (!as64bit && !spec.empty()) {
                                spec.insert(spec.size() - 1, "l");
                            }
                            double dumpAsDouble = 0.;
                            resf = fscanf(filepointer, spec.c_str(), &dumpAsDouble);
                            if (resf == EOF || resf == 0) {
                                bContinue = false;
                            } else {
                                if (outType == AS_NONE) {
                                    outType = AS_DOUBLE;
                                } else {
                                    if (outType != AS_DOUBLE) {
                                        outType = AS_MIXED;
                                    }
                                }
                                values.push_back(ArrayOf::doubleScalar(dumpAsDouble));
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
                            bool asChar = (*(np - 1) == 'c');
                            std::string charset;
                            bool bOK = scanfReadCharsOrString(filepointer, dp, asChar, charset);
                            if (!bOK) {
                                bContinue = false;
                                break;
                            }
                            std::string asUtf8;
                            charsetToUtf8Converter(charset, encoding, asUtf8);
                            values.push_back(ArrayOf::characterArray(asUtf8));
                        } break;
                        default: {
                            if (buff) {
                                delete[] buff;
                                buff = nullptr;
                            }
                            Error(L"Nelson:stream_manager:unsupportedFormat",
                                L"Unsupported %s format.", L"fscanf");
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
        if (values.size() == valuesBefore && ftell(filepointer) == passStart) {
            // a pass that consumed nothing (empty or unmatched format) would
            // repeat forever: stop here.
            bContinue = false;
        }
    }
    count = values.size();
    ArrayOf value;
    if (count == 0) {
        Dimensions empty(0, 0);
        value = ArrayOf::empty(empty);
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
            std::wstring str = value.asWideString();
            strs.append(str);
        }
        value = ArrayOf::characterArray(strs);
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
            switch (value.dataClass()) {
            case NLS_UINT64:
            case NLS_INT64:
            case NLS_DOUBLE: {
                v.push_back(value.asDoubleScalar());
            } break;
            case NLS_CHAR: {
                value.promoteType(NLS_DOUBLE);
                Dimensions dimsValue = value.dimensions();
                v.reserve(v.size() + dimsValue.elementCount());
                double* ptr = (double*)value.dataPointer();
                indexType elementCount = dimsValue.elementCount();
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
} // namespace Nelson
//=============================================================================
