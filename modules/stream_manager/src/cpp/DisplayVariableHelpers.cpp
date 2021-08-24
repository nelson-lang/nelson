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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <cstring>
#include <limits>
#include "DisplayVariableHelpers.hpp"
#include "DisplayFloatingNumber.hpp"
#include "NelsonConfiguration.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
outputDoublePrecisionFloat(double num);
//=============================================================================
static std::string
outputSinglePrecisionFloat(single num);
//=============================================================================
static void
summarizeStringArray(Interface* io, const ArrayOf& A);
//=============================================================================
static void
summarizeCellEntry(Interface* io, const ArrayOf& A);
//=============================================================================
static void
emitElement(Interface* io, char* msgBuffer, const void* dp, indexType num, Class dcls);
//=============================================================================
static void
printMe(Interface* io, const ArrayOf& A);
//=============================================================================
void
DisplayVariableHeader(Interface* io, const ArrayOf& A, const std::string& name)
{
    if (!name.empty()) {
        io->outputMessage("\n");
        io->outputMessage(name + " =\n\n");
    }
}
//=============================================================================
void
DisplayVariableFooter(Interface* io, const ArrayOf& A, const std::string& name)
{
    if (!name.empty()) {
        io->outputMessage("\n");
    }
}
//=============================================================================
void
DisplayVariableValue(Interface* io, const ArrayOf& A, const std::string& name)
{
    switch (A.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        if (A.isNdArrayDoubleType() || A.isNdArraySingleType()) {
            printMe(io, A);
            return;
        }
        DisplayFloatingNumber(io, A, name);
    } break;
    case NLS_CHAR: {
        if (A.isRowVectorCharacterArray()) {
            std::wstring msg = A.getContentAsWideString();
            if (msg.empty()) {
                if (name.empty()) {
                    io->outputMessage("");
                } else {
                    io->outputMessage("''\n");
                }
            } else {
                if (name.empty()) {
                    io->outputMessage(msg + L"\n");
                } else {
                    io->outputMessage(L"\'" + msg + L"\'\n");
                }
            }
        } else {
            printMe(io, A);
        }
    } break;
    default: {
        printMe(io, A);
    } break;
    }
}
//=============================================================================
std::string
outputDoublePrecisionFloat(double num)
{
    std::string str;
    if (num >= 0) {
        str = fmt::sprintf(" ");
    }
    if (IsInfinite(num)) {
        str = fmt::sprintf("   Inf");
    } else if (IsNaN(num)) {
        str = fmt::sprintf("   NaN");
    } else if ((fabs(num) >= 0.1f && fabs(num) < 1.0f)
        || num <= std::numeric_limits<single>::epsilon()) {
        str = fmt::sprintf("  %0.15f", num);
    } else if (fabs(num) >= 0.01f && fabs(num) < 0.1f) {
        str = fmt::sprintf("  %0.16f", num);
    } else if (fabs(num) >= 0.001f && fabs(num) < 0.01f) {
        str = fmt::sprintf("  %0.17f", num);
    } else if (fabs(num) >= 1.0f && fabs(num) < 10.0f) {
        str = fmt::sprintf("  %0.15f", num);
    } else if (fabs(num) >= 10.0f && fabs(num) < 100.0f) {
        str = fmt::sprintf("  %2.13f", num);
    } else if (fabs(num) >= 100.0f && fabs(num) < 1000.0f) {
        str = fmt::sprintf("%3.12f", num);
    } else {
        str = fmt::sprintf(" % 1.14e", num);
    }
    size_t len = str.length();
    str.append(24 - len, ' ');
    return str;
}
//=============================================================================
std::string
outputSinglePrecisionFloat(single num)
{
    std::string str;
    if (num >= 0) {
        str = fmt::sprintf(" ");
    }
    if (IsNaN(num)) {
        str = fmt::sprintf("   NaN");
    } else if ((fabs(num) >= 0.1f && fabs(num) < 1.0f)
        || num <= std::numeric_limits<single>::epsilon()) {
        str = fmt::sprintf("  %0.8f", num);
    } else if (fabs(num) >= 0.01f && fabs(num) < 0.1f) {
        str = fmt::sprintf("  %0.9f", num);
    } else if (fabs(num) >= 0.001f && fabs(num) < 0.01f) {
        str = fmt::sprintf("  %0.10f", num);
    } else if (fabs(num) >= 1.0f && fabs(num) < 10.0f) {
        str = fmt::sprintf("  %1.7f", num);
    } else if (fabs(num) >= 10.0f && fabs(num) < 100.0f) {
        str = fmt::sprintf(" %2.6f", num);
    } else if (fabs(num) >= 100.0f && fabs(num) < 1000.0f) {
        str = fmt::sprintf("%3.5f", num);
    } else {
        str = fmt::sprintf("  %1.7e", num);
    }
    size_t len = str.length();
    str.append(24 - len, ' ');
    return str;
}
//=============================================================================
void
summarizeStringArray(Interface* io, const ArrayOf& A)
{
    if (A.isEmpty()) {
        if (A.isCharacterArray()) {
            io->outputMessage("\"\"");
        } else {
            io->outputMessage("<missing>");
        }
    } else {
        if (A.getDataClass() == NLS_DOUBLE) {
            bool isOk = false;
            if (A.isScalar()) {
                double* v = (double*)A.getDataPointer();
                if (std::isnan(v[0])) {
                    io->outputMessage("<missing>");
                    isOk = true;
                }
            }
            if (!isOk) {
                Error(_W("character array expected."));
            }
        } else if (A.getDataClass() == NLS_CHAR) {
            Dimensions dims = A.getDimensions();
            if (dims.isRowVector()) {
                if (dims.getColumns() < static_cast<indexType>(io->getTerminalWidth() - 3)) {
                    std::wstring str = A.getContentAsWideString();
                    str = L"\"" + str + L"\"";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dims.printMe(io);
            io->outputMessage(" string]");
        } else {
            Error(_W("character array expected."));
        }
    }
}
//=============================================================================
/**
 * Print this object when it is an element of a cell array.  This is
 * generally a shorthand summary of the description of the object.
 */
void
summarizeCellEntry(Interface* io, const ArrayOf& A)
{
    if (A.isEmpty()) {
        if (A.getDataPointer() == nullptr) {
            io->outputMessage("[]");
        } else {
            if (A.getDataClass() == NLS_CHAR) {
                io->outputMessage("''");
            } else {
                io->outputMessage("[]");
            }
        }
    } else {
        switch (A.getDataClass()) {
        case NLS_CELL_ARRAY:
            io->outputMessage("{");
            A.getDimensions().printMe(io);
            io->outputMessage(" cell }");
            break;
        case NLS_STRING_ARRAY:
            io->outputMessage("[");
            A.getDimensions().printMe(io);
            io->outputMessage(" string ]");
            break;
        case NLS_STRUCT_ARRAY:
            io->outputMessage(" ");
            A.getDimensions().printMe(io);
            if (A.getStructType() == NLS_FUNCTION_HANDLE_STR) {
                io->outputMessage(std::string(" ") + NLS_FUNCTION_HANDLE_STR);
            } else if (A.getStructType() == NLS_STRUCT_ARRAY_STR) {
                io->outputMessage(" struct array");
            } else {
                io->outputMessage(std::string(" class ") + A.getStructType());
            }
            break;
        case NLS_CHAR: {
            Dimensions dims = A.getDimensions();
            if (dims.isRowVector()) {
                if (dims.getColumns() < static_cast<indexType>(io->getTerminalWidth() - 3)) {
                    std::wstring str = A.getContentAsWideString();
                    str = L"\'" + str + L"\'";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dims.printMe(io);
            io->outputMessage(" string]");
        } break;
        case NLS_GO_HANDLE:
            if (A.isScalar()) {
                io->outputMessage("[graphic_object]");
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" graphic_object]");
            }
            break;
        case NLS_HANDLE:
            if (A.isScalar()) {
                io->outputMessage("[handle]");
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" handle]");
            }
            break;
        case NLS_LOGICAL:
            if (!A.isSparse() && A.isScalar()) {
                io->outputMessage(
                    fmt::sprintf("[%d]", *(static_cast<const logical*>(A.getSparseDataPointer()))));
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                if (A.isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" logical]");
            }
            break;
        case NLS_UINT8:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%d]", *(static_cast<const uint8*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" uint8]");
            }
            break;
        case NLS_INT8:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%d]", *(static_cast<const int8*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" int8]");
            }
            break;
        case NLS_UINT16:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%d]", *(static_cast<const uint16*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" uint16]");
            }
            break;
        case NLS_INT16:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%d]", *(static_cast<const int16*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" int16]");
            }
            break;
        case NLS_UINT32:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%d]", *(static_cast<const uint32*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" uint32]");
            }
            break;
        case NLS_INT32:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%d]", *(static_cast<const int32*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" int32]");
            }
            break;
        case NLS_UINT64: {
            if (A.isScalar()) {
                uint64 val = *(static_cast<const uint64*>(A.getDataPointer()));
                std::string msg = "[" + std::to_string(val) + "]";
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" uint64]");
            }
        } break;
        case NLS_INT64: {
            if (A.isScalar()) {
                int64 value = *(static_cast<const int64*>(A.getDataPointer()));
                std::string msg = std::string("[") + std::to_string(value) + std::string("]");
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" int64]");
            }
        } break;
        case NLS_DOUBLE:
            if (!A.isSparse() && A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%lf]", *(static_cast<const double*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                if (A.isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" double]");
            }
            break;
        case NLS_DCOMPLEX:
            if (!A.isSparse() && A.isScalar()) {
                const auto* ap = static_cast<const double*>(A.getDataPointer());
                std::string msg = fmt::sprintf("[%lf+%lfi]", ap[0], ap[1]);
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                if (A.isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" dcomplex]");
            }
            break;
        case NLS_SINGLE:
            if (A.isScalar()) {
                std::string msg
                    = fmt::sprintf("[%f]", *(static_cast<const single*>(A.getDataPointer())));
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" single]");
            }
            break;
        case NLS_SCOMPLEX:
            if (A.isScalar()) {
                const auto* ap = static_cast<const single*>(A.getDataPointer());
                std::string msg = fmt::sprintf("[%f+%fi]", ap[0], ap[1]);
                io->outputMessage(msg);
            } else {
                io->outputMessage("[");
                A.getDimensions().printMe(io);
                io->outputMessage(" complex]");
            }
            break;
        default: { } break; }
    }
}
//=============================================================================
void
emitElement(Interface* io, const void* dp, indexType num, Class dcls)
{
    switch (dcls) {
    case NLS_STRUCT_ARRAY: {
    } break;
    case NLS_GO_HANDLE: {
    } break;
    case NLS_HANDLE: {
    } break;
    case NLS_INT8: {
        const int8* ap = static_cast<const int8*>(dp);
        io->outputMessage(fmt::sprintf("% 4d", ap[num]));
        io->outputMessage("  ");
    } break;
    case NLS_UINT8: {
        const auto* ap = static_cast<const uint8*>(dp);
        uint8 value = ap[num];
        io->outputMessage(fmt::sprintf("%3u", value));
        io->outputMessage("  ");
    } break;
    case NLS_INT16: {
        const auto* ap = static_cast<const int16*>(dp);
        int16 value = ap[num];
        io->outputMessage(fmt::sprintf("% 6d", value));
        io->outputMessage("  ");
    } break;
    case NLS_UINT16: {
        const auto* ap = static_cast<const uint16*>(dp);
        uint16 value = ap[num];
        io->outputMessage(fmt::sprintf("%5u", value));
        io->outputMessage("  ");
    } break;
    case NLS_INT32: {
        const auto* ap = static_cast<const int32*>(dp);
        int32 value = ap[num];
        io->outputMessage(fmt::sprintf("%13d", value));
        io->outputMessage("  ");
    } break;
    case NLS_UINT32: {
        const auto* ap = static_cast<const uint32*>(dp);
        uint32 value = ap[num];
        io->outputMessage(fmt::sprintf("%12u", value));
        io->outputMessage("  ");
    } break;
    case NLS_INT64: {
        const auto* ap = static_cast<const int64*>(dp);
        std::string msg = std::to_string(ap[num]) + "  ";
        io->outputMessage(msg);
    } break;
    case NLS_UINT64: {
        const auto* ap = static_cast<const uint64*>(dp);
        std::string msg;
        msg = std::to_string(ap[num]) + "  ";
        io->outputMessage(msg);
    } break;
    case NLS_LOGICAL: {
        const auto* ap = static_cast<const logical*>(dp);
        if (ap[num] == 0) {
            io->outputMessage("false  ");
        } else {
            io->outputMessage("true   ");
        }
    } break;
    case NLS_CHAR: {
        const auto* ap = static_cast<const charType*>(dp);
        std::wstring wstr;
        wstr.push_back(ap[num]);
        io->outputMessage(wstr);
    } break;
    case NLS_SINGLE: {
        const auto* ap = static_cast<const single*>(dp);
        io->outputMessage(outputSinglePrecisionFloat(ap[num]));
        io->outputMessage("  ");
    } break;
    case NLS_DOUBLE: {
        const auto* ap = static_cast<const double*>(dp);
        io->outputMessage(outputDoublePrecisionFloat(ap[num]));
        io->outputMessage("  ");
    } break;
    case NLS_SCOMPLEX: {
        const auto* ap = static_cast<const single*>(dp);
        io->outputMessage(outputSinglePrecisionFloat(ap[2 * num]));
        io->outputMessage(" ");
        io->outputMessage(outputSinglePrecisionFloat(ap[2 * num + 1]));
        io->outputMessage("i  ");
    } break;
    case NLS_DCOMPLEX: {
        const auto* ap = static_cast<const double*>(dp);
        io->outputMessage(outputDoublePrecisionFloat(ap[2 * num]));
        io->outputMessage(" ");
        io->outputMessage(outputDoublePrecisionFloat(ap[2 * num + 1]));
        io->outputMessage("i  ");
    } break;
    case NLS_CELL_ARRAY: {
        auto* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            io->outputMessage("[]");
        } else {
            summarizeCellEntry(io, ap[num]);
        }
    } break;
    case NLS_STRING_ARRAY: {
        auto* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            io->outputMessage("[]");
        } else {
            summarizeStringArray(io, ap[num]);
        }
    } break;
    default: { } break; }
}
//=============================================================================
/**
 * Display this variable on the given output stream.
 */
void
printMe(Interface* io, const ArrayOf& A)
{
    int nominalWidth;
    sizeType termWidth = io->getTerminalWidth();
    std::string typeAsText = "";
    switch (A.getDataClass()) {
    case NLS_GO_HANDLE:
        typeAsText = "  <graphic_object>  ";
        nominalWidth = 5;
        break;
    case NLS_HANDLE:
        typeAsText = "  <handle>  ";
        nominalWidth = 5;
        break;
    case NLS_UINT8:
        typeAsText = "  <uint8>  ";
        nominalWidth = 5;
        break;
    case NLS_INT8:
        typeAsText = "  <int8> ";
        nominalWidth = 6;
        break;
    case NLS_UINT16:
        typeAsText = "  <uint16>  ";
        nominalWidth = 7;
        break;
    case NLS_INT16:
        typeAsText = "  <int16> ";
        nominalWidth = 8;
        break;
    case NLS_UINT32:
        typeAsText = "  <uint32>  ";
        nominalWidth = 14;
        break;
    case NLS_INT32:
        typeAsText = "  <int32>  ";
        nominalWidth = 15;
        break;
    case NLS_UINT64:
        typeAsText = "  <uint64>  ";
        nominalWidth = 14;
        break;
    case NLS_INT64:
        typeAsText = "  <int64>  ";
        nominalWidth = 15;
        break;
    case NLS_SINGLE:
        typeAsText = "  <single>  ";
        nominalWidth = 20;
        break;
    case NLS_DOUBLE:
        typeAsText = "  <double>  ";
        nominalWidth = 30;
        break;
    case NLS_LOGICAL:
        typeAsText = "  <logical>  ";
        nominalWidth = 2;
        break;
    case NLS_CHAR:
        typeAsText = "  <char>  ";
        nominalWidth = 1;
        break;
    case NLS_SCOMPLEX:
        typeAsText = "  <single>  ";
        nominalWidth = 36;
        break;
    case NLS_DCOMPLEX:
        typeAsText = "  <double>  ";
        nominalWidth = 54;
        break;
    case NLS_CELL_ARRAY:
        typeAsText = "  <cell> ";
        nominalWidth = 10;
        break;
    case NLS_STRUCT_ARRAY: {
        if (A.isClassStruct()) {
            typeAsText = "  <" + A.getStructType() + "> ";
        } else {
            typeAsText = "  <struct> ";
        }
        nominalWidth = 10;
    } break;
    case NLS_STRING_ARRAY:
        typeAsText = "  <string> ";
        nominalWidth = 10;
        break;
    default: { } break; }

    io->outputMessage(typeAsText + "- size: ");
    A.getDimensions().printMe(io);
    io->outputMessage("\n\n");
    if (A.isEmpty()) {
        if (A.isStruct()) {
            stringVector fieldsName = A.getFieldNames();
            if (fieldsName.empty()) {
                io->outputMessage("  []\n");
            } else {
                for (const auto& k : fieldsName) {
                    io->outputMessage("    ");
                    io->outputMessage(k);
                    io->outputMessage("\n");
                }
            }
        } else {
            io->outputMessage("  []\n");
        }
        return;
    }
    if (A.isSparse()) {
        std::string msg
            = fmt::sprintf(_("\tMatrix is sparse with %d nonzeros\n").c_str(), A.getNonzeros());
        io->outputMessage(msg);
        return;
    }
    if (A.getDataClass() == NLS_STRUCT_ARRAY) {
        if (A.isScalar()) {
            ArrayOf* ap;
            ap = (ArrayOf*)A.getDataPointer();
            for (sizeType n = 0; n < static_cast<sizeType>(A.getFieldNames().size()); n++) {
                io->outputMessage("    ");
                io->outputMessage(A.getFieldNames()[n]);
                io->outputMessage(": ");
                summarizeCellEntry(io, ap[n]);
                io->outputMessage("\n");
            }
        } else {
            if (!A.getFieldNames().empty()) {
                io->outputMessage("  Fields\n");
                for (const auto& fieldName : A.getFieldNames()) {
                    io->outputMessage("    ");
                    io->outputMessage(fieldName);
                    io->outputMessage("\n");
                }
            }
        }
    } else {
        Dimensions dims = A.getDimensions();
        const void* ap = A.getDataPointer();
        if (dims.getLength() == 2) {
            indexType rows = dims.getRows();
            indexType columns = dims.getColumns();
            // Determine how many columns will fit across
            // the terminal width
            auto colsPerPage = static_cast<indexType>(
                floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
            auto pageCount
                = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
            for (indexType k = 0;
                 k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {
                indexType colsInThisPage = columns - colsPerPage * k;
                colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                if (A.getElementCount() > 1 && A.getDataClass() != NLS_CHAR) {
                    std::string msg = fmt::sprintf(_("\nColumns %d to %d\n").c_str(),
                        k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                    io->outputMessage(msg);
                }
                for (indexType i = 0; i < rows; i++) {
                    if (A.getDataClass() == NLS_CHAR) {
                        io->outputMessage(" '");
                    } else {
                        io->outputMessage(" ");
                    }
                    for (indexType j = 0; j < colsInThisPage; j++) {
                        emitElement(io, ap, i + (k * colsPerPage + j) * rows, A.getDataClass());
                        if ((j < colsInThisPage - 1) && A.getDataClass() != NLS_CHAR) {
                            io->outputMessage(" ");
                        }
                    }
                    if (A.getDataClass() == NLS_CHAR) {
                        io->outputMessage("'\n");
                    } else {
                        io->outputMessage("\n");
                    }
                }
            }
        } else if (dims.getLength() > 2) {
            /**
             * For N-ary arrays, data slice  -  start with
             * [1,1,1,...,1].  We keep doing the matrix
             * print , incrementing from the highest dimension,
             * and rolling downwards.
             */
            Dimensions wdims(dims.getLength());
            indexType rows(A.getRows());
            indexType columns(A.getColumns());
            indexType offset = 0;
            while (wdims.inside(dims)) {
                io->outputMessage("(:,:");
                for (indexType m = 2; m < dims.getLength(); m++) {
                    io->outputMessage(fmt::sprintf(",%d", static_cast<int>(wdims[m]) + 1));
                }
                io->outputMessage(") =\n\n");
                // Determine how many columns will fit across
                // the terminal width
                auto colsPerPage = static_cast<indexType>(
                    floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
                int pageCount;
                pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
                for (int k = 0;
                     k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending();
                     k++) {
                    indexType colsInThisPage = columns - colsPerPage * k;
                    colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                    io->outputMessage(fmt::sprintf(_("\nColumns %d to %d\n"), k * colsPerPage + 1,
                        k * colsPerPage + colsInThisPage));
                    for (indexType i = 0; i < rows; i++) {
                        io->outputMessage(" ");
                        if (A.getDataClass() == NLS_CHAR) {
                            io->outputMessage("'");
                        }
                        for (indexType j = 0; j < colsInThisPage; j++) {
                            emitElement(io, ap, i + (k * colsPerPage + j) * rows + offset,
                                A.getDataClass());
                        }
                        if (A.getDataClass() == NLS_CHAR) {
                            io->outputMessage("'");
                        }
                        io->outputMessage("\n");
                    }
                }
                offset += rows * columns;
                wdims.incrementModulo(dims, 2);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
