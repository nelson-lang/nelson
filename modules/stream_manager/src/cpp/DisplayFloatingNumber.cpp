//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include <memory> // For std::unique_ptr
#include <cstdarg> // For va_start, etc.
#include "DisplayFloatingNumber.hpp"
#include "Error.hpp"
#include "StringFormat.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
isInteger(T val)
{
    uint64_t valAsInt = *reinterpret_cast<uint64_t*>(&val);
    int exponent = ((valAsInt >> 52) & 0x7FF) - 1023;
    int bitsInFraction = 52 - exponent;
    uint64_t mask
        = exponent < 0 ? 0x7FFFFFFFFFFFFFFFLL : exponent > 52 ? 0x00 : (1LL << bitsInFraction) - 1;
    return !(valAsInt & mask);
}
//=============================================================================
template <class T>
static bool
IsIntegerValues(ArrayOf A, T& minVal, T& maxVal)
{
    bool res = true;
    Dimensions dimsA = A.getDimensions();
    if (A.isComplex()) {
        T* pValueA = (T*)A.getDataPointer();
        if (pValueA) {
            maxVal = pValueA[0];
            minVal = pValueA[0];
            for (indexType k = 0; k < dimsA.getElementCount() * 2; k++) {
                if (!isInteger(pValueA[k])) {
                    return false;
                } else {
                    if (maxVal < pValueA[k]) {
                        maxVal = pValueA[k];
                    }
                    if (minVal > pValueA[k]) {
                        minVal = pValueA[k];
                    }
                }
            }
        } else {
            return false;
        }

    } else // NLS_DCOMPLEX
    {
        T* pValueA = (T*)A.getDataPointer();
        if (pValueA) {
            maxVal = pValueA[0];
            for (indexType k = 0; k < dimsA.getElementCount(); k++) {
                if (!isInteger(pValueA[k])) {
                    return false;
                } else {
                    if (maxVal < pValueA[k]) {
                        maxVal = pValueA[k];
                    }
                    if (minVal > pValueA[k]) {
                        minVal = pValueA[k];
                    }
                }
            }
        } else {
            return false;
        }
    }
    return res;
}
//=============================================================================
template <class T>
static std::wstring
printNumber(T number, OutputFormatDisplay currentFormat, bool asInteger, bool asScalar)
{
    std::wstring strNumber = L"";
    strNumber.reserve(64);
    if (std::isfinite(number)) {
        switch (currentFormat) {
        case NLS_FORMAT_SHORT: {
            if (asInteger) {
                if (fabs(number) < 1e9) {
                    strNumber = std::to_wstring((int64)number);
                } else {
                    strNumber = StringFormat(
                        L"%*.*e", 9, 4, number); // lgtm [cpp/wrong-type-format-argument]
                }
            } else {
                if (fabs(number) > 1e-4 || number == 0. || !asScalar) {
                    strNumber = StringFormat(
                        L"%*.*f", 9, 4, number); // lgtm [cpp/wrong-type-format-argument]
                } else {
                    strNumber = StringFormat(
                        L"%*.*e", 9, 4, number); // lgtm [cpp/wrong-type-format-argument]
                }
            }
        } break;
        case NLS_FORMAT_LONG: {
            if (asInteger) {
                if (fabs(number) < 1e9) {
                    strNumber = std::to_wstring((int64)number);
                } else {
                    strNumber = StringFormat(
                        L"%*.*e", 18, 15, number); // lgtm [cpp/wrong-type-format-argument]
                }
            } else {
                if (fabs(number) > 1e-9 || number == 0.) {
                    strNumber = StringFormat(
                        L"%*.*f", 18, 15, number); // lgtm [cpp/wrong-type-format-argument]
                } else {
                    strNumber = StringFormat(
                        L"%*.*e", 18, 15, number); // lgtm [cpp/wrong-type-format-argument]
                }
            }
        } break;
        case NLS_FORMAT_SHORTE: {
            strNumber
                = StringFormat(L"%*.*e", 10, 4, number); // lgtm [cpp/wrong-type-format-argument]
        } break;
        case NLS_FORMAT_LONGE: {
            strNumber
                = StringFormat(L"%*.*e", 23, 15, number); // lgtm [cpp/wrong-type-format-argument]
        } break;
        case NLS_FORMAT_HEX: {
            strNumber = double2hexastr(number);
        } break;
        }
    } else {
        if (std::isnan(number)) {
            strNumber = L" NaN";
        } else {
            if (number > 0) {
                strNumber = L" Inf";
            } else {
                strNumber = L"-Inf";
            }
        }
    }
    return strNumber;
}
//=============================================================================
template <class T>
static std::wstring
printNumber(
    T realpart, T imagpart, OutputFormatDisplay currentFormat, bool asInteger, bool asScalar)
{
    std::wstring strNumber = L"";
    strNumber.reserve(128);
    if (imagpart >= 0.0 || std::isnan(imagpart)) {
        std::wstring realStr = printNumber(realpart, currentFormat, asInteger, asScalar);
        strNumber.append(realStr);
        strNumber.append(L" + ");
        std::wstring imagStr = printNumber(imagpart, currentFormat, asInteger, asScalar);
        boost::algorithm::trim_left(imagStr);
        strNumber.append(imagStr);
        strNumber.append(L"i");
    } else {
        std::wstring realStr = printNumber(realpart, currentFormat, asInteger, asScalar);
        strNumber.append(realStr);
        strNumber.append(L" - ");
        std::wstring imagStr = printNumber(abs(imagpart), currentFormat, asInteger, asScalar);
        boost::algorithm::trim_left(imagStr);
        strNumber.append(imagStr);
        strNumber.append(L"i");
    }
    return strNumber;
}
//=============================================================================
template <class T>
void
DisplayFloatingNumberInternal(
    Interface* io, const ArrayOf& A, bool fromDispBuiltin, bool& needToOverload)
{
    Dimensions dimsA = A.getDimensions();
    indexType termWidth = io->getTerminalWidth();
    if (A.isEmpty()) {
        if (A.isEmpty(true)) {
            io->outputMessage(L"     []\n");
        } else {
            dimsA.simplify();
            std::wstring msg = _W("   Empty matrix : ");
            for (indexType k = 0; k < dimsA.getLength(); ++k) {
                msg = msg + std::to_wstring(dimsA.getDimensionLength(k));
                if (k < dimsA.getLength() - 1) {
                    msg = msg + _W("-by-");
                }
            }
            msg = msg + L"\n";
            io->outputMessage(msg);
        }
        return;
    }
    T maxFloatingNumber = 0;
    T minFloatingNumber = 0;
    bool asInteger = IsIntegerValues(A, minFloatingNumber, maxFloatingNumber);
    indexType columns = dimsA.getColumns();
    indexType rows = dimsA.getRows();
    T* pValueA = (T*)A.getDataPointer();
    if (A.isScalar()) {
        io->outputMessage(L"  ");
        std::wstring strNumber;
        if (A.isComplex()) {
            std::complex<T>* cplx = reinterpret_cast<std::complex<T>*>(pValueA);
            strNumber = printNumber<T>(cplx->real(), cplx->imag(),
                NelsonConfiguration::getInstance()->getOutputFormatDisplay(), false, true);
        } else {
            strNumber = printNumber<T>(pValueA[0],
                NelsonConfiguration::getInstance()->getOutputFormatDisplay(), asInteger, true);
        }
        io->outputMessage(strNumber);
        io->outputMessage(L"\n");
    } else // matrix
    {
        indexType format_width = 8;
        bool bIsComplex = A.isComplex();
        switch (NelsonConfiguration::getInstance()->getOutputFormatDisplay()) {
        case NLS_FORMAT_SHORT: {
            if (asInteger && !bIsComplex) {
                if (fabs(minFloatingNumber) > fabs(maxFloatingNumber)) {
                    std::wstring str = std::to_wstring((int64)minFloatingNumber);
                    format_width = str.size() + 1;
                } else {
                    std::wstring str = std::to_wstring((int64)maxFloatingNumber);
                    format_width = str.size() + 3;
                }
            } else {
                if (bIsComplex) {
                    format_width = 10 * 2 + wcslen(L" + ") + wcslen(L"i");
                } else {
                    format_width = 10;
                }
            }
        } break;
        case NLS_FORMAT_LONG:
            if (asInteger && !bIsComplex) {
                if (fabs(minFloatingNumber) > fabs(maxFloatingNumber)) {
                    std::wstring str = std::to_wstring((int64)minFloatingNumber);
                    format_width = str.size() + 1;
                } else {
                    std::wstring str = std::to_wstring((int64)maxFloatingNumber);
                    format_width = str.size() + 3;
                }
            } else {
                if (bIsComplex) {
                    format_width = 18 * 2 + wcslen(L" + ") + wcslen(L"i");
                } else {
                    format_width = 18;
                }
            }
            break;
        case NLS_FORMAT_SHORTE:
            if (asInteger && !bIsComplex) {
                if (fabs(minFloatingNumber) > fabs(maxFloatingNumber)) {
                    std::wstring str = std::to_wstring((int64)minFloatingNumber);
                    format_width = str.size() + 1;
                } else {
                    std::wstring str = std::to_wstring((int64)maxFloatingNumber);
                    format_width = str.size() + 3;
                }
            } else {
                if (bIsComplex) {
                    format_width = 10 * 2 + wcslen(L" + ") + wcslen(L"i");
                } else {
                    format_width = 10;
                }
            }
            break;
        case NLS_FORMAT_LONGE:
            if (asInteger && !bIsComplex) {
                if (fabs(minFloatingNumber) > fabs(maxFloatingNumber)) {
                    std::wstring str = std::to_wstring((int64)minFloatingNumber);
                    format_width = str.size() + 1;
                } else {
                    std::wstring str = std::to_wstring((int64)maxFloatingNumber);
                    format_width = str.size() + 3;
                }
            } else {
                if (bIsComplex) {
                    format_width = 23 * 2 + wcslen(L" + ") + wcslen(L"i");
                } else {
                    format_width = 23;
                }
            }
            break;
        case NLS_FORMAT_HEX:
            format_width = 16;
            break;
        }
        indexType colsPerPage = (indexType)floor((termWidth - 1) / ((double)format_width + 2));
        colsPerPage = (colsPerPage < 1) ? 1 : colsPerPage;
        indexType pageCount = (indexType)ceil(columns / ((double)colsPerPage));
        std::wstring buffer;
        try {
            buffer.reserve(20 * columns);
        } catch (const std::bad_alloc&) {
        }
        indexType block_page = 0;
        bool continueDisplay = true;
        for (indexType k = 0; k < pageCount && continueDisplay; k++) {
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                continueDisplay = false;
                break;
            }
            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if ((rows * columns > 1) && (pageCount > 1)) {
                std::wstring msg = StringFormat(_W("\n  Columns %d to %d\n\n").c_str(),
                    (k * colsPerPage + 1), (k * colsPerPage + colsInThisPage));
                buffer.append(msg);
            }
            for (indexType i = 0; i < rows && continueDisplay; i++) {
                buffer.append(L"  ");
                for (indexType j = 0; j < colsInThisPage; j++) {
                    if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                        continueDisplay = false;
                        break;
                    }
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    std::wstring numberAsStr;
                    if (bIsComplex) {
                        numberAsStr = printNumber(pValueA[2 * idx], pValueA[2 * idx + 1],
                            NelsonConfiguration::getInstance()->getOutputFormatDisplay(), false,
                            false);
                    } else {
                        numberAsStr = printNumber(pValueA[idx],
                            NelsonConfiguration::getInstance()->getOutputFormatDisplay(), asInteger,
                            false);
                        size_t len = numberAsStr.size();
                        if (len < (size_t)format_width) {
                            size_t nb = format_width - len;
                            for (size_t q = 0; q < nb; q++) {
                                buffer.append(L" ");
                            }
                        }
                    }
                    buffer.append(numberAsStr);
                    if (j < colsInThisPage - 1) {
                        buffer.append(L"  ");
                    }
                }
                buffer.append(L"\n");
                if (block_page > termWidth) {
                    io->outputMessage(buffer);
                    buffer.clear();
                    block_page = 0;
                } else {
                    block_page++;
                }
            }
            if (!buffer.empty()) {
                io->outputMessage(buffer);
                buffer.clear();
                block_page = 0;
            }
        }
    }
}
//=============================================================================
void
DisplayFloatingNumber(Interface* io, const ArrayOf& A, bool fromDispBuiltin, bool& needToOverload)
{
    if (A.isNdArrayDoubleType() || A.isNdArraySingleType()) {
        A.printMe(io);
        return;
    }
    if (A.isSingleClass()) {
        DisplayFloatingNumberInternal<single>(io, A, fromDispBuiltin, needToOverload);
    } else {
        DisplayFloatingNumberInternal<double>(io, A, fromDispBuiltin, needToOverload);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
