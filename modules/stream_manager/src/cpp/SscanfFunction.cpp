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
#include <fstream>
#include "StringHelpers.hpp"
#include "FileTell.hpp"
#include "FileWrite.hpp"
#include "SscanfFunction.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
flagChar(wchar_t c)
{
    return ((c == L'#') || (c == L'0') || (c == L'-') || (c == L' ') || (c == L'+'));
}
//=============================================================================
static int
convspec(wchar_t c)
{
    return ((c == L'd') || (c == L'i') || (c == L'o') || (c == L'u') || (c == L'x') || (c == L'e')
        || (c == L'f') || (c == L'g') || (c == L'c') || (c == L's'));
}
//=============================================================================
static wchar_t*
validateScanFormatSpec(wchar_t* cp)
{
    if (*cp == L'%') {
        return cp + 1;
    }
    while ((*cp) && flagChar(*cp)) {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    while ((*cp) && (*cp == L'.')) {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    if ((*cp) && (convspec(*cp) || (*cp == L'l'))) {
        return cp + 1;
    }
    return nullptr;
}
//=============================================================================
template <class T>
ArrayOf
convertToArrayOfSscanf(ArrayOfVector& values, NelsonType classDestination, bool haveThirdArgument,
    double m, double n, bool mixed)
{
    ArrayOf value;
    Dimensions nDims;

    indexType M = 0;
    indexType N = 0;

    if (mixed) {
        ArrayOf v = values[0];
        M = v.getRows();
        N = v.getColumns();
    } else {
        if (!haveThirdArgument) {
            M = values.size();
            N = 1;
        } else {
            if (std::isinf(n)) {
                M = (indexType)m;
                N = (indexType)(values.size() / m);
            } else {
                M = (indexType)m;
                N = (indexType)n;
            }
        }
    }
    Dimensions dims(M, N);
    T* ptr = (T*)ArrayOf::allocateArrayOf(
        classDestination, M * N, stringVector(), M * N != (indexType)values.size());
    value = ArrayOf(classDestination, dims, ptr);
    indexType minLength = std::min(M * N, mixed ? M * N : (indexType)values.size());

    if (mixed) {
        ArrayOf v = values[0];
        T* src = (T*)v.getDataPointer();
        std::memcpy(ptr, src, sizeof(T) * minLength);
    } else {
        switch (classDestination) {
        case NLS_UINT64: {
            for (indexType k = 0; k < minLength; k++) {
                ptr[k] = (T)values[k].getContentAsUnsignedInteger64Scalar();
            }
        } break;
        case NLS_INT64: {
            for (indexType k = 0; k < minLength; k++) {
                ptr[k] = (T)values[k].getContentAsInteger64Scalar();
            }
        } break;
        case NLS_DOUBLE:
        default: {
            for (indexType k = 0; k < minLength; k++) {
                ptr[k] = (T)values[k].getContentAsDoubleScalar();
            }
        } break;
        }
    }
    return value;
}
//=============================================================================
enum OutputType
{
    AS_STRING,
    AS_DOUBLE,
    AS_INT64,
    AS_UINT64,
    AS_MIXED,
    AS_NONE
};
//=============================================================================
static OutputType
getCommonOutputType(const std::vector<OutputType>& types)
{
    OutputType commonOutputType = AS_NONE;
    for (auto outputType : types) {
        if (commonOutputType == AS_NONE) {
            commonOutputType = outputType;
        } else {
            if (commonOutputType != outputType) {
                commonOutputType = AS_MIXED;
            }
        }
    }
    return commonOutputType;
}
//=============================================================================
static bool
fwscanfAsInt64(FILE* filepointer, const std::wstring& fmt, int64& value, indexType& nextIndex,
    std::wstring& errorMessage)
{
    int pos = 0;
    long sdumint64 = 0;
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), &sdumint64, &pos);
    indexType o2 = NLSFTELL(filepointer);
    value = (int64)sdumint64;
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (feof(filepointer) || resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _W("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fwscanfAsCharacterVector(FILE* filepointer, const std::wstring& fmt, bool asChar,
    std::wstring& value, indexType& nextIndex, std::wstring& errorMessage)
{
    int pos = 0;
    wchar_t buffer[8192];
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), buffer, &pos);
    indexType o2 = NLSFTELL(filepointer);
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (asChar) {
        buffer[resf] = 0;
    }
    value = buffer;
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _W("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fwscanfAsUInt64(FILE* filepointer, const std::wstring& fmt, uint64& value, indexType& nextIndex,
    std::wstring& errorMessage)
{
    int pos = 0;
    unsigned long sdumint64 = 0;
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), &sdumint64, &pos);
    indexType o2 = NLSFTELL(filepointer);
    value = (uint64)sdumint64;
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _W("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fwscanfUnsignedIntegerAsDouble(FILE* filepointer, const std::wstring& fmt, double& value,
    indexType& nextIndex, std::wstring& errorMessage)
{
    int pos = 0;
    unsigned int dumpUnsignedInt = 0;
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), &dumpUnsignedInt, &pos);
    indexType o2 = NLSFTELL(filepointer);
    value = (double)dumpUnsignedInt;
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (resf == EOF || resf == 0) {
        errorMessage = _W("Matching failure in format.");
        return false;
    }
    return true;
}
//=============================================================================
static bool
fwscanfIntegerAsDouble(FILE* filepointer, const std::wstring& fmt, double& value,
    indexType& nextIndex, std::wstring& errorMessage)
{
    int pos = 0;
    int sdumdouble = 0;
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), &sdumdouble, &pos);
    indexType o2 = NLSFTELL(filepointer);
    value = (double)sdumdouble;
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _W("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fwscanfAsDouble(FILE* filepointer, const std::wstring& fmt, double& value, indexType& nextIndex,
    std::wstring& errorMessage)
{
    int pos = 0;
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), &value, &pos);
    indexType o2 = NLSFTELL(filepointer);
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _W("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fwscanfSingleAsDouble(FILE* filepointer, const std::wstring& fmt, double& value,
    indexType& nextIndex, std::wstring& errorMessage)
{
    int pos = 0;
    single dumpAsFloat = single(0.);
    indexType o1 = NLSFTELL(filepointer);
    int resf = fwscanf(filepointer, fmt.c_str(), &dumpAsFloat, &pos);
    indexType o2 = NLSFTELL(filepointer);
    value = (double)dumpAsFloat;
    if (pos == 0) {
        nextIndex += (o2 - o1);
    } else {
        nextIndex += pos;
    }
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _W("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static ArrayOf
FwscanF(FILE* filepointer, const std::wstring& format, double m, double n, bool haveThirdArgument,
    indexType& count, indexType& nextIndex, std::wstring& errorMessage)
{
    OutputType outType = AS_NONE;
    ArrayOfVector values;
    bool bContinue = true;
    int resf = 0;
    std::vector<OutputType> outTypes;
    std::wstring fmt;
    int pos = 0;
    nextIndex = 1;
    std::wstring fmtPosition = L"%n";
    while (bContinue) {
        if (feof(filepointer)) {
            bContinue = false;
            break;
        }
        wchar_t* buff = nullptr;
        try {
            buff = new wchar_t[format.size() + 1];
        } catch (std::bad_alloc&) {
            errorMessage = ERROR_MEMORY_ALLOCATION;
            return {};
        }
        wcscpy(buff, format.c_str());
        wchar_t* dp = buff;
        wchar_t* np;
        wchar_t sv;
        while (*dp) {
            pos = 0;
            np = dp;
            while ((*dp) && (*dp != L'%')) {
                dp++;
            }
            sv = *dp;
            *dp = 0;
            if (wcslen(np) == 0) {
                fmt = fmtPosition;
            } else {
                fmt = std::wstring(np);
                fmt += fmtPosition;
            }
            indexType o1 = NLSFTELL(filepointer);
            resf = fwscanf(filepointer, fmt.c_str(), &pos);
            indexType o2 = NLSFTELL(filepointer);
            if (pos == 0) {
                nextIndex += (o2 - o1);
            } else {
                nextIndex += pos;
            }
            pos = 0;
            if (feof(filepointer)) {
                bContinue = false;
            }
            *dp = sv;
            if (*dp) {
                np = validateScanFormatSpec(dp + 1);
                if (!np) {
                    delete[] buff;
                    errorMessage = _W("Invalid format.");
                    goto endLoopWscanf;
                } else {
                    if (*(np - 1) == L'%') {
                        pos = 0;
                        indexType o1 = NLSFTELL(filepointer);
                        resf = fwscanf(filepointer, L"%%%n", &pos);
                        indexType o2 = NLSFTELL(filepointer);
                        if (pos == 0) {
                            nextIndex += (o2 - o1);
                        } else {
                            nextIndex += pos;
                        }
                        if (resf == EOF) {
                            bContinue = false;
                        }
                        dp += 2;
                    } else {
                        bool as64bit = false;
                        if (*(np - 1) == L'l') {
                            as64bit = true;
                            np++;
                        }
                        sv = *np;
                        *np = 0;
                        switch (*(np - 1)) {
                        case L'd':
                        case L'i': {
                            fmt = std::wstring(dp);
                            fmt += fmtPosition;
                            if (as64bit) {
                                if (outType == AS_NONE) {
                                    outType = AS_INT64;
                                } else {
                                    if (outType != AS_INT64) {
                                        outType = AS_MIXED;
                                    }
                                }
                                outTypes.push_back(outType);
                                int64 value = 0;
                                bool bOK = fwscanfAsInt64(
                                    filepointer, fmt, value, nextIndex, errorMessage);
                                if (!bOK) {
                                    bContinue = false;
                                    break;
                                }
                                values.push_back(ArrayOf::int64Constructor(value));
                            } else {
                                if (outType == AS_NONE) {
                                    outType = AS_DOUBLE;
                                } else {
                                    if (outType != AS_DOUBLE) {
                                        outType = AS_MIXED;
                                    }
                                }
                                outTypes.push_back(outType);
                                double value = 0.;
                                bool bOK = fwscanfIntegerAsDouble(
                                    filepointer, fmt, value, nextIndex, errorMessage);
                                if (!bOK) {
                                    bContinue = false;
                                    break;
                                }
                                values.push_back(ArrayOf::doubleConstructor(value));
                            }
                        } break;
                        case L'o':
                        case L'u':
                        case L'x': {
                            fmt = std::wstring(dp);
                            fmt += fmtPosition;
                            if (as64bit) {
                                if (outType == AS_NONE) {
                                    outType = AS_UINT64;
                                } else {
                                    if (outType != AS_UINT64) {
                                        outType = AS_MIXED;
                                    }
                                }
                                outTypes.push_back(outType);
                                uint64 value = (uint64)0;
                                bool bOK = fwscanfAsUInt64(
                                    filepointer, fmt, value, nextIndex, errorMessage);
                                if (!bOK) {
                                    bContinue = false;
                                    break;
                                }
                                values.push_back(ArrayOf::uint64Constructor(value));
                            } else {
                                if (outType == AS_NONE) {
                                    outType = AS_DOUBLE;
                                } else {
                                    if (outType != AS_DOUBLE) {
                                        outType = AS_MIXED;
                                    }
                                }
                                outTypes.push_back(outType);
                                double value = 0.;
                                bool bOK = fwscanfUnsignedIntegerAsDouble(
                                    filepointer, fmt, value, nextIndex, errorMessage);
                                if (!bOK) {
                                    errorMessage = _W("Matching failure in format.");
                                    bContinue = false;
                                    break;
                                }
                                values.push_back(ArrayOf::doubleConstructor(value));
                            }
                        } break;
                        case L'e':
                        case L'f':
                        case L'g': {
                            if (outType == AS_NONE) {
                                outType = AS_DOUBLE;
                            } else {
                                if (outType != AS_DOUBLE) {
                                    outType = AS_MIXED;
                                }
                            }
                            outTypes.push_back(outType);
                            fmt = std::wstring(dp);
                            fmt += fmtPosition;
                            double value = 0;
                            bool bOK = false;
                            if (as64bit) {
                                bOK = fwscanfAsDouble(
                                    filepointer, fmt, value, nextIndex, errorMessage);
                            } else {
                                bOK = fwscanfSingleAsDouble(
                                    filepointer, fmt, value, nextIndex, errorMessage);
                            }
                            if (!bOK) {
                                bContinue = false;
                                break;
                            }
                            values.push_back(ArrayOf::doubleConstructor(value));
                        } break;
                        case L'c':
                        case L's': {
                            if (outType == AS_NONE) {
                                outType = AS_STRING;
                            } else {
                                if (outType != AS_STRING) {
                                    outType = AS_MIXED;
                                }
                            }
                            outTypes.push_back(outType);
                            if (as64bit) {
                                delete[] buff;
                                errorMessage = _W("Invalid format.");
                                goto endLoopWscanf;
                            }
                            bool asChar = (*(np - 1) == L'c');
                            fmt = std::wstring(dp);
                            if (asChar) {
                                StringHelpers::replace_first(fmt, L"c", L"lc");
                            } else {
                                StringHelpers::replace_first(fmt, L"s", L"ls");
                            }
                            fmt += fmtPosition;
                            std::wstring value;
                            bool bOK = fwscanfAsCharacterVector(
                                filepointer, fmt, asChar, value, nextIndex, errorMessage);
                            if (!bOK) {
                                bContinue = false;
                                break;
                            }
                            values.push_back(ArrayOf::characterArrayConstructor(value));
                        } break;
                        default: {
                            if (buff) {
                                delete[] buff;
                                buff = nullptr;
                            }
                            errorMessage = _W("Unsupported sscanf format.");
                            goto endLoopWscanf;
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

endLoopWscanf:
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
    outType = getCommonOutputType(outTypes);
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
        value = convertToArrayOfSscanf<double>(values, NLS_DOUBLE, haveThirdArgument, m, n, false);
    } break;
    case AS_INT64: {
        value = convertToArrayOfSscanf<int64>(values, NLS_INT64, haveThirdArgument, m, n, false);
    } break;
    case AS_UINT64: {
        value = convertToArrayOfSscanf<uint64>(values, NLS_UINT64, haveThirdArgument, m, n, false);
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
        value = convertToArrayOfSscanf<double>(promoted, NLS_DOUBLE, haveThirdArgument, m, n, true);
    } break;
    case AS_NONE: {
    } break;
    default: {
    } break;
    }
    return value;
}
//=============================================================================
ArrayOf
SscanF(const std::wstring& content, const std::wstring& format, double m, double n,
    bool haveThirdArgument, indexType& count, indexType& nextIndex, std::wstring& errorMessage)
{
    FileSystemWrapper::Path tempFilePath = FileSystemWrapper::Path::unique_path();
#ifdef _MSC_VER
    const std::wstring filenameTemp = tempFilePath.wstring();
#else
    const std::wstring filenameTemp = utf8_to_wstring(tempFilePath.string());
#endif
    wstringVector lines;
    lines.push_back(content);
    if (!writeFile(filenameTemp, lines, L"\n", "\n", "UTF-8", errorMessage)) {
        return {};
    }
#ifdef _MSC_VER
    FILE* fp = _wfopen(filenameTemp.c_str(), L"rt, ccs=UTF-8");
#else
    FILE* fp = fopen(wstring_to_utf8(filenameTemp).c_str(), "rt");
#endif
    if (!fp) {
        Error(_W("sscanf internal error."));
        return {};
    }
    ArrayOf value = FwscanF(fp, format, m, n, haveThirdArgument, count, nextIndex, errorMessage);
    if (fp) {
        fclose(fp);
        fp = nullptr;
    }
#ifdef _MSC_VER
    _wremove(filenameTemp.c_str());
#else
    remove(wstring_to_utf8(filenameTemp).c_str());
#endif
    return value;
}
//=============================================================================
}
//=============================================================================
