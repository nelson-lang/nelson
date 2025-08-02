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
#include <cstring>
#include <fstream>
#include "Fscanf_helpers.hpp"
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
fscanfAsInt64(FILE* filepointer, const std::string& fmt, int64& value, std::string& errorMessage)
{
    int pos = 0;
    long sdumint64 = 0;
    int resf = fscanf(filepointer, fmt.c_str(), &sdumint64, &pos);
    value = (int64)sdumint64;
    if (feof(filepointer) || resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fscanfAsCharacterVector(FILE* filepointer, const std::string& fmt, bool asChar, std::string& value,
    std::string& errorMessage)
{
    int pos = 0;
    char buffer[8192];
    int resf = fscanf(filepointer, fmt.c_str(), buffer, &pos);
    if (asChar) {
        buffer[resf] = 0;
    }
    value = buffer;
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fscanfAsUInt64(FILE* filepointer, const std::string& fmt, uint64& value, std::string& errorMessage)
{
    int pos = 0;
    unsigned long sdumint64 = 0;
    int resf = fscanf(filepointer, fmt.c_str(), &sdumint64, &pos);
    value = (uint64)sdumint64;
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fscanfUnsignedIntegerAsDouble(
    FILE* filepointer, const std::string& fmt, double& value, std::string& errorMessage)
{
    int pos = 0;
    unsigned int dumpUnsignedInt = 0;
    int resf = fscanf(filepointer, fmt.c_str(), &dumpUnsignedInt, &pos);
    value = (double)dumpUnsignedInt;
    if (resf == EOF || resf == 0) {
        errorMessage = _("Matching failure in format.");
        return false;
    }
    return true;
}
//=============================================================================
static bool
fscanfIntegerAsDouble(
    FILE* filepointer, const std::string& fmt, double& value, std::string& errorMessage)
{
    int pos = 0;
    int sdumdouble = 0;
    int resf = fscanf(filepointer, fmt.c_str(), &sdumdouble, &pos);
    value = (double)sdumdouble;
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fscanfAsDouble(FILE* filepointer, const std::string& fmt, double& value, std::string& errorMessage)
{
    int pos = 0;
    int resf = fscanf(filepointer, fmt.c_str(), &value, &pos);
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static bool
fscanfSingleAsDouble(
    FILE* filepointer, const std::string& fmt, double& value, std::string& errorMessage)
{
    int pos = 0;
    single dumpAsFloat = single(0.);
    int resf = fscanf(filepointer, fmt.c_str(), &dumpAsFloat, &pos);
    value = (double)dumpAsFloat;
    if (resf == EOF || resf == 0) {
        if (resf == 0) {
            errorMessage = _("Matching failure in format.");
        }
        return false;
    }
    return true;
}
//=============================================================================
static ArrayOf
fscanfInternal(FILE* filepointer, const std::string& format, double m, double n,
    bool haveThirdArgument, indexType& count, std::string& errorMessage)
{
    OutputType outType = AS_NONE;
    ArrayOfVector values;
    bool bContinue = true;
    int resf = 0;
    std::vector<OutputType> outTypes;
    std::string fmt;
    int pos = 0;
    std::string fmtPosition = "%n";
    while (bContinue) {
        if (feof(filepointer)) {
            bContinue = false;
            break;
        }
        char* buff = nullptr;
        try {
            buff = new char[format.size() + 1];
        } catch (std::bad_alloc&) {
            errorMessage = wstring_to_utf8(ERROR_MEMORY_ALLOCATION);
            return {};
        }
        strcpy(buff, format.c_str());
        char* dp = buff;
        char* np;
        char sv;
        while (*dp) {
            pos = 0;
            np = dp;
            while ((*dp) && (*dp != '%')) {
                dp++;
            }
            sv = *dp;
            *dp = 0;
            if (strlen(np) == 0) {
                fmt = fmtPosition;
            } else {
                fmt = std::string(np);
                fmt += fmtPosition;
            }
            resf = fscanf(filepointer, fmt.c_str(), &pos);
            pos = 0;
            if (feof(filepointer)) {
                bContinue = false;
            }
            *dp = sv;
            if (*dp) {
                np = validateScanFormatSpec(dp + 1);
                if (!np) {
                    delete[] buff;
                    errorMessage = _("Invalid format.");
                    goto endLoopWscanf;
                } else {
                    if (*(np - 1) == '%') {
                        pos = 0;
                        resf = fscanf(filepointer, "%%%n", &pos);
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
                            fmt = std::string(dp);
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
                                bool bOK = fscanfAsInt64(filepointer, fmt, value, errorMessage);
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
                                bool bOK
                                    = fscanfIntegerAsDouble(filepointer, fmt, value, errorMessage);
                                if (!bOK) {
                                    bContinue = false;
                                    break;
                                }
                                values.push_back(ArrayOf::doubleConstructor(value));
                            }
                        } break;
                        case 'o':
                        case 'u':
                        case 'x': {
                            fmt = std::string(dp);
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
                                bool bOK = fscanfAsUInt64(filepointer, fmt, value, errorMessage);
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
                                bool bOK = fscanfUnsignedIntegerAsDouble(
                                    filepointer, fmt, value, errorMessage);
                                if (!bOK) {
                                    errorMessage = _("Matching failure in format.");
                                    bContinue = false;
                                    break;
                                }
                                values.push_back(ArrayOf::doubleConstructor(value));
                            }
                        } break;
                        case 'e':
                        case 'f':
                        case 'g': {
                            if (outType == AS_NONE) {
                                outType = AS_DOUBLE;
                            } else {
                                if (outType != AS_DOUBLE) {
                                    outType = AS_MIXED;
                                }
                            }
                            outTypes.push_back(outType);
                            fmt = std::string(dp);
                            fmt += fmtPosition;
                            double value = 0;
                            bool bOK = false;
                            if (as64bit) {
                                bOK = fscanfAsDouble(filepointer, fmt, value, errorMessage);
                            } else {
                                bOK = fscanfSingleAsDouble(filepointer, fmt, value, errorMessage);
                            }
                            if (!bOK) {
                                bContinue = false;
                                break;
                            }
                            values.push_back(ArrayOf::doubleConstructor(value));
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
                            outTypes.push_back(outType);
                            if (as64bit) {
                                delete[] buff;
                                errorMessage = _("Invalid format.");
                                goto endLoopWscanf;
                            }
                            bool asChar = (*(np - 1) == 'c');
                            fmt = std::string(dp);
                            fmt += fmtPosition;
                            std::string value;
                            bool bOK = fscanfAsCharacterVector(
                                filepointer, fmt, asChar, value, errorMessage);
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
                            errorMessage = _("Unsupported sscanf format.");
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
    bool haveThirdArgument, indexType& count, indexType& nextIndex, bool withNextIndex,
    std::wstring& errorMessage)
{
    FileSystemWrapper::Path tempFilePath = FileSystemWrapper::Path::unique_path();
#ifdef _MSC_VER
    const std::wstring wfilenameTemp = tempFilePath.wstring();
    const std::string filenameTemp = wstring_to_utf8(wfilenameTemp);
#else
    const std::string filenameTemp = tempFilePath.string();
    const std::wstring wfilenameTemp = utf8_to_wstring(filenameTemp);
#endif
    wstringVector lines;
    lines.push_back(content);
    if (!writeFile(wfilenameTemp, lines, L"\n", "\n", "UTF-8", errorMessage)) {
        return {};
    }
#ifdef _MSC_VER
    FILE* fp = _wfopen(wfilenameTemp.c_str(), L"rt");
#else
    FILE* fp = fopen(filenameTemp.c_str(), "rt");
#endif
    if (!fp) {
        Error(_W("sscanf internal error."));
        return {};
    }
    std::string _errorMessage;
    indexType initialFilePos = NLSFTELL(fp);
    ArrayOf value = fscanfInternal(
        fp, wstring_to_utf8(format), m, n, haveThirdArgument, count, _errorMessage);
    indexType latestFilePos = NLSFTELL(fp);

    if (withNextIndex) {
        nextIndex = (latestFilePos - initialFilePos);
        std::string contentUtf8 = wstring_to_utf8(content);
        size_t wideLen = content.substr(0, nextIndex).length();
        size_t utf8Len = contentUtf8.substr(0, nextIndex).length();
        nextIndex = wideLen + 1;
    } else {
        nextIndex = 0;
    }
    if (!_errorMessage.empty()) {
        errorMessage = utf8_to_wstring(_errorMessage);
    }
    if (fp) {
        fclose(fp);
        fp = nullptr;
    }
#ifdef _MSC_VER
    _wremove(wfilenameTemp.c_str());
#else
    remove(filenameTemp.c_str());
#endif
    return value;
}
//=============================================================================
}
//=============================================================================
