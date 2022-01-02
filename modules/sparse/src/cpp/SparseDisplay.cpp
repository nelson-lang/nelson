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
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <boost/format.hpp>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include "characters_encoding.hpp"
#include "SparseDisplay.hpp"
#include "NelsonConfiguration.hpp"
#include "IEEEFP.hpp"
#include "Interface.hpp"
#include "SparseType.hpp"
#include "DisplayVariableHelpers.hpp"
//=============================================================================
using boost::io::group;
using std::hex;
using std::setfill;
using std::setw;
using std::showbase;
//=============================================================================
#ifdef _MSC_VER
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
single2hexastr(single d)
{
    char* buffer = new char[32];
    sprintf(buffer, "%X", *reinterpret_cast<int*>(&d));
    return buffer;
}
//=============================================================================
std::string
double2hexastr(double d)
{
    char* buffer = new char[32];
    sprintf(buffer, "%llx", *reinterpret_cast<unsigned long long*>(&d));
    return buffer;
}
//=============================================================================
static std::string
printNumber(double number, NumericFormatDisplay currentFormat)
{
    std::string strNumber;
    boost::format fmtnbr;
    if (IsInfinite(number)) {
        if (number > 0) {
            strNumber = " Inf";
        } else {
            strNumber = "-Inf";
        }
    } else {
        if (IsNaN(number)) {
            strNumber = " NaN";
        } else {
            switch (currentFormat) {
            case NLS_NUMERIC_FORMAT_SHORT:
                fmtnbr = boost::format("%1$10.4f");
                if (number == 0.0) {
                    strNumber = str(fmtnbr % 0.0);
                } else {
                    strNumber = str(fmtnbr % number);
                }
                break;
            case NLS_NUMERIC_FORMAT_LONG:
                fmtnbr = boost::format("%1$20.16g");
                if (number == 0.0) {
                    strNumber = str(fmtnbr % 0.0);
                } else {
                    strNumber = str(fmtnbr % number);
                }
                break;
            case NLS_NUMERIC_FORMAT_SHORTE:
                fmtnbr = boost::format("%1$10.5e");
                if (number == 0.0) {
                    strNumber = str(fmtnbr % 0.0);
                } else {
                    strNumber = str(fmtnbr % number);
                }
                break;
            case NLS_NUMERIC_FORMAT_LONGE:
                fmtnbr = boost::format("%1$10.5le");
                if (number == 0.0) {
                    strNumber = str(fmtnbr % 0.0);
                } else {
                    strNumber = str(fmtnbr % number);
                }
                break;
            case NLS_NUMERIC_FORMAT_HEX:
                strNumber = double2hexastr(number);
                break;
            }
        }
    }
    return strNumber;
}
//=============================================================================
static std::string
printNumber(double realpart, double imagpart, NumericFormatDisplay currentFormat)
{
    std::string strNumber;
    if (imagpart != 0.0) {
        if (imagpart > 0.0) {
            strNumber = printNumber(realpart, currentFormat) + " + "
                + printNumber(imagpart, currentFormat) + "i";
        } else {
            strNumber = printNumber(realpart, currentFormat) + " - "
                + printNumber(abs(imagpart), currentFormat) + "i";
        }
    } else {
        strNumber = printNumber(realpart, currentFormat);
    }
    return strNumber;
}
//=============================================================================
void
SparseDoubleDisplay(Interface* io, const ArrayOf& a)
{
    size_t rows = a.getDimensionLength(0);
    size_t cols = a.getDimensionLength(1);
    if ((rows == 0) || (cols == 0)) {
        std::string msg
            = str(boost::format(_("\tAll zero sparse: %d-by-%d\n")) % (int)rows % (int)cols);
        io->outputMessage(msg);
    } else {
        if (a.getNonzeros() == 0) {
            std::string msg = str(boost::format(_("\tAll zero sparse: %lu-by-%lu\n"))
                % (long long)rows % (long long)cols);
            io->outputMessage(msg);
        } else {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)a.getSparseDataPointer();
            for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                    break;
                }
                for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spMat, k);
                     it; ++it) {
                    if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                        break;
                    }
                    std::string strNumber = printNumber(
                        it.value(), NelsonConfiguration::getInstance()->getNumericFormatDisplay());
                    std::string msg
                        = str(boost::format("\t(%lu,%lu)\t%s\n") % (long long)(it.row() + 1)
                            % (long long)(it.col() + 1) % strNumber.c_str());
                    io->outputMessage(msg);
                }
            }
        }
    }
}
//=============================================================================
void
SparseDoubleComplexDisplay(Interface* io, const ArrayOf& a)
{
    const double** src = (const double**)a.getSparseDataPointer();
    size_t rows = a.getDimensionLength(0);
    size_t cols = a.getDimensionLength(1);
    if (rows == 0 || cols == 0) {
        std::string msg
            = str(boost::format(_("\tAll zero sparse: %d-by-%d\n")) % (int)rows % (int)cols);
        io->outputMessage(msg);
    } else {
        if (a.getNonzeros() == 0) {
            std::string msg = str(boost::format(_("\tAll zero sparse: %lu-by-%lu\n"))
                % (long long)rows % (long long)cols);
            io->outputMessage(msg);
        } else {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)a.getSparseDataPointer();
            for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                    break;
                }
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spMat, k);
                     it; ++it) {
                    if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                        break;
                    }
                    std::string strNumber = printNumber(it.value().real(), it.value().imag(),
                        NelsonConfiguration::getInstance()->getNumericFormatDisplay());
                    std::string msg
                        = str(boost::format("\t(%lu,%lu)\t%s\n") % (long long)(it.row() + 1)
                            % (long long)(it.col() + 1) % strNumber.c_str());
                    io->outputMessage(msg);
                }
            }
        }
    }
}
//=============================================================================
void
SparseLogicalDisplay(Interface* io, const ArrayOf& a)
{
    const logical** src = (const logical**)a.getSparseDataPointer();
    size_t rows = a.getDimensionLength(0);
    size_t cols = a.getDimensionLength(1);
    if (rows == 0 || cols == 0) {
        std::string msg
            = str(boost::format(_("\tAll zero sparse: %d-by-%d\n")) % (int)rows % (int)cols);
        io->outputMessage(msg);
    } else {
        if (a.getNonzeros() == 0) {
            std::string msg = str(boost::format(_("\tAll zero sparse: %lu-by-%lu\n"))
                % (long long)rows % (long long)cols);
            io->outputMessage(msg);
        } else {
            Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)a.getSparseDataPointer();
            for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                    break;
                }
                for (Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator it(*spMat, k);
                     it; ++it) {
                    if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                        break;
                    }
                    std::string msg = "";
                    if (it.value()) {
                        msg = str(boost::format("\t(%lu,%lu) true\n") % (long long)(it.row() + 1)
                            % (long long)(it.col() + 1));
                    } else {
                        msg = str(boost::format("\t(%lu,%lu) false\n") % (long long)(it.row() + 1)
                            % (long long)(it.col() + 1));
                    }
                    io->outputMessage(msg);
                }
            }
        }
    }
}
//=============================================================================
void
SparseDisplay(Interface* io, const ArrayOf& a, const std::string& name)
{
    DisplayVariableHeader(io, a, utf8_to_wstring(name));
    switch (a.getDataClass()) {
    case NLS_DOUBLE:
        SparseDoubleDisplay(io, a);
        break;
    case NLS_DCOMPLEX:
        SparseDoubleComplexDisplay(io, a);
        break;
    case NLS_LOGICAL:
        SparseLogicalDisplay(io, a);
        break;
    default:
        break;
    }
    DisplayVariableFooter(io, a, utf8_to_wstring(name));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
