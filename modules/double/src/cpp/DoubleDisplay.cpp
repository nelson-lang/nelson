//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <iomanip>
#include <cstdio>
#include <stdarg.h>  // For va_start, etc.
#include <memory>    // For std::unique_ptr
#include "DoubleDisplay.hpp"
#include "StringFormat.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    std::wstring double2hexastr(double d)
    {
        char *buffer = new char[32];
        sprintf(buffer, "%llx", *(unsigned long long *)&d);
        return utf8_to_wstring(buffer);
    }
    //=============================================================================
    static std::wstring printNumber(double number, OutputFormatDisplay currentFormat)
    {
        std::wstring strNumber = L"";
        strNumber.reserve(64);
        if (std::isfinite(number))
        {
            switch (currentFormat)
            {
                case NLS_FORMAT_SHORT:
                {
                    strNumber = StringFormat(L"%9.4f", number);
                }
                break;
                case NLS_FORMAT_LONG:
                {
                    strNumber = StringFormat(L"%18.15f", number);
                }
                break;
                case NLS_FORMAT_SHORTE:
                {
                    strNumber = StringFormat(L"%10.4e", number);
                }
                break;
                case NLS_FORMAT_LONGE:
                {
                    strNumber = StringFormat(L"%23.15e", number);
                }
                break;
                case NLS_FORMAT_HEX:
                {
                    strNumber = double2hexastr(number);
                }
                break;
            }
        }
        else
        {
            if (std::isnan(number))
            {
                strNumber = L" NaN";
            }
            else
            {
                if (number > 0)
                {
                    strNumber = L" Inf";
                }
                else
                {
                    strNumber = L"-Inf";
                }
            }
        }
        return strNumber;
    }
    //=============================================================================
    static std::wstring printNumber(double realpart, double imagpart, OutputFormatDisplay currentFormat)
    {
        std::wstring strNumber = L"";
        strNumber.reserve(128);
        if (imagpart != 0.0)
        {
            if (imagpart > 0.0)
            {
                strNumber.append(printNumber(realpart, currentFormat));
                strNumber.append(L" +");
                strNumber.append(printNumber(imagpart, currentFormat));
                strNumber.append(L"i");
            }
            else
            {
                strNumber.append(printNumber(realpart, currentFormat));
                strNumber.append(L" -");
                strNumber.append(printNumber(abs(imagpart), currentFormat));
                strNumber.append(L"i");
            }
        }
        else
        {
            strNumber = printNumber(realpart, currentFormat);
        }
        return strNumber;
    }
    //=============================================================================
    void DoubleDisplay(Evaluator *eval, ArrayOf A)
    {
        Dimensions dimsA = A.getDimensions();
        Interface *io = eval->getInterface();
        indexType termWidth = io->getTerminalWidth();
        if (A.isEmpty())
        {
            if (A.isEmpty(true))
            {
                io->outputMessage(L"     []\n");
            }
            else
            {
                dimsA.simplify();
                std::wstring msg = _W("   Empty matrix : ");
                for (indexType k = 0; k < dimsA.getLength(); ++k)
                {
                    msg = msg + std::to_wstring(dimsA.getDimensionLength(k));
                    if (k < dimsA.getLength() - 1)
                    {
                        msg = msg + _W("-by-");
                    }
                }
                msg = msg + L"\n";
                io->outputMessage(msg);
            }
            return;
        }
        else
        {
            indexType columns = dimsA.getColumns();
            indexType  rows = dimsA.getRows();
            const double *pValueA = (const double*)A.getDataPointer();
            if (A.isScalar())
            {
                io->outputMessage(L"  ");
                std::wstring strNumber;
                if (A.isComplex())
                {
                    strNumber = printNumber(pValueA[0], pValueA[1], eval->getCurrentOutputFormatDisplay());
                }
                else
                {
                    strNumber = printNumber(pValueA[0], eval->getCurrentOutputFormatDisplay());
                }
                io->outputMessage(strNumber);
                io->outputMessage(L"\n");
            }
            else // matrix
            {
                indexType format_width = 8;
                bool bIsComplex = A.isComplex();
                switch (eval->getCurrentOutputFormatDisplay())
                {
                    case NLS_FORMAT_SHORT:
                        format_width = 10;
                        break;
                    case NLS_FORMAT_LONG:
                        format_width = 18;
                        break;
                    case NLS_FORMAT_SHORTE:
                        format_width = 10;
                        break;
                    case NLS_FORMAT_LONGE:
                        format_width = 23;
                        break;
                    case NLS_FORMAT_HEX:
                        format_width = 16;
                        break;
                }
                if (bIsComplex)
                {
                    format_width = format_width * 2 + wcslen(L" +") + wcslen(L"i");
                }
                indexType colsPerPage = (indexType)floor((termWidth - 1) / ((double)format_width + 2));
                colsPerPage = (colsPerPage < 1) ? 1 : colsPerPage;
                indexType pageCount = (indexType)ceil(columns / ((double)colsPerPage));
                std::wstring buffer;
                buffer.reserve(1024 * rows * columns);
                indexType block_page = 0;
                for (indexType k = 0; k < pageCount; k++)
                {
                    if (eval->GetInterruptPending())
                    {
                        break;
                    }
                    indexType colsInThisPage = columns - colsPerPage * k;
                    colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                    if ((rows * columns > 1) && (pageCount > 1))
                    {
                        std::wstring msg = StringFormat(_W("\n  Columns %d to %d\n\n").c_str(), (k * colsPerPage + 1), (k * colsPerPage + colsInThisPage));
                        buffer.append(msg);
                    }
                    for (indexType i = 0; i < rows; i++)
                    {
                        buffer.append(L"  ");
                        for (indexType j = 0; j < colsInThisPage; j++)
                        {
                            indexType idx = i + (k * colsPerPage + j) * rows;
                            bIsComplex ? buffer.append(printNumber(pValueA[2 * idx], pValueA[2 * idx + 1], eval->getCurrentOutputFormatDisplay())) : buffer.append(printNumber(pValueA[idx], eval->getCurrentOutputFormatDisplay()));
                            buffer.append(L"  ");
                        }
                        buffer.append(L"\n");
                        if (block_page > termWidth)
                        {
                            io->outputMessage(buffer);
                            buffer = L"";
                            block_page = 0;
                        }
                        else
                        {
                            block_page++;
                        }
                    }
                    if (!buffer.empty())
                    {
                        io->outputMessage(buffer);
                        buffer = L"";
                        block_page = 0;
                    }
                }
            }
        }
    }
    //=============================================================================
}
//=============================================================================
