//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include <set>
#include <cstring>
#include <limits>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "IEEEFP.hpp"
#include "Interface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Interface* io;
//=============================================================================
typedef std::set<uint32, std::less<uint32>> intSet;
intSet addresses;
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
void
outputDoublePrecisionFloat(char* buf, double num)
{
    char temp_buf[100];
    char* tbuf;
    sizeType len;
    tbuf = temp_buf;
    if (num >= 0) {
        sprintf(tbuf, " ");
        tbuf++;
    }
    if (IsInfinite(num)) {
        sprintf(tbuf, "   Inf");
    } else if (IsNaN(num)) {
        sprintf(tbuf, "   NaN");
    } else if ((fabs(num) >= 0.1f && fabs(num) < 1.0f)
        || num <= std::numeric_limits<single>::epsilon()) {
        sprintf(tbuf, "  %0.15f", num);
    } else if (fabs(num) >= 0.01f && fabs(num) < 0.1f) {
        sprintf(tbuf, "  %0.16f", num);
    } else if (fabs(num) >= 0.001f && fabs(num) < 0.01f) {
        sprintf(tbuf, "  %0.17f", num);
    } else if (fabs(num) >= 1.0f && fabs(num) < 10.0f) {
        sprintf(tbuf, "  %1.15f", num);
    } else if (fabs(num) >= 10.0f && fabs(num) < 100.0f) {
        sprintf(tbuf, " %2.13f", num);
    } else if (fabs(num) >= 100.0f && fabs(num) < 1000.0f) {
        sprintf(tbuf, "%3.12f", num);
    } else {
        sprintf(tbuf, "  %1.14e", num);
    }
    len = strlen(temp_buf);
    memcpy(buf, temp_buf, len);
    memset(buf + len, ' ', 24 - len);
    buf[24] = 0;
}
//=============================================================================
void
outputSinglePrecisionFloat(char* buf, single num)
{
    char temp_buf[100];
    char* tbuf;
    sizeType len;
    tbuf = temp_buf;
    if (num >= 0) {
        sprintf(tbuf, " ");
        tbuf++;
    }
    if (IsNaN(num)) {
        sprintf(tbuf, "   NaN");
    } else if ((fabs(num) >= 0.1f && fabs(num) < 1.0f)
        || num <= std::numeric_limits<single>::epsilon()) {
        sprintf(tbuf, "  %0.8f", num);
    } else if (fabs(num) >= 0.01f && fabs(num) < 0.1f) {
        sprintf(tbuf, "  %0.9f", num);
    } else if (fabs(num) >= 0.001f && fabs(num) < 0.01f) {
        sprintf(tbuf, "  %0.10f", num);
    } else if (fabs(num) >= 1.0f && fabs(num) < 10.0f) {
        sprintf(tbuf, "  %1.7f", num);
    } else if (fabs(num) >= 10.0f && fabs(num) < 100.0f) {
        sprintf(tbuf, " %2.6f", num);
    } else if (fabs(num) >= 100.0f && fabs(num) < 1000.0f) {
        sprintf(tbuf, "%3.5f", num);
    } else {
        sprintf(tbuf, "  %1.7e", num);
    }
    len = strlen(temp_buf);
    memcpy(buf, temp_buf, len);
    memset(buf + len, ' ', 17 - len);
    buf[17] = 0;
}
//=============================================================================
void
ArrayOf::summarizeStringArray(Interface* io) const
{
    if (isEmpty()) {
        if (isCharacterArray()) {
            io->outputMessage("\"\"");
        } else {
            io->outputMessage("<missing>");
        }
    } else {
        if (dp->dataClass == NLS_CHAR) {
            Dimensions dims = dp->dimensions;
            if (dims.isRowVector()) {
                if (dims.getColumns() < (indexType)(io->getTerminalWidth() - 3)) {
                    std::wstring str = getContentAsWideString();
                    str = L"\"" + str + L"\"";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dp->dimensions.printMe(io);
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
ArrayOf::summarizeCellEntry(Interface* io) const
{
    if (isEmpty()) {
        if (dp->dataClass == NLS_CHAR) {
            io->outputMessage("''");
        } else {
            io->outputMessage("[]");
        }
    } else {
        switch (dp->dataClass) {
        case NLS_CELL_ARRAY:
            io->outputMessage("{");
            dp->dimensions.printMe(io);
            io->outputMessage(" cell }");
            break;
        case NLS_STRING_ARRAY:
            io->outputMessage("[");
            dp->dimensions.printMe(io);
            io->outputMessage(" string ]");
            break;
        case NLS_STRUCT_ARRAY:
            io->outputMessage(" ");
            dp->dimensions.printMe(io);
            if (dp->getStructTypeName() == NLS_FUNCTION_HANDLE_STR) {
                io->outputMessage(std::string(" ") + NLS_FUNCTION_HANDLE_STR);
            } else if (dp->getStructTypeName() == NLS_STRUCT_ARRAY_STR) {
                io->outputMessage(" struct array");
            } else {
                io->outputMessage(std::string(" class ") + dp->getStructTypeName());
            }
            break;
        case NLS_CHAR: {
            Dimensions dims = dp->dimensions;
            if (dims.isRowVector()) {
                if (dims.getColumns() < (indexType)(io->getTerminalWidth() - 3)) {
                    std::wstring str = getContentAsWideString();
                    str = L"\'" + str + L"\'";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dp->dimensions.printMe(io);
            io->outputMessage(" string]");
        } break;
        case NLS_HANDLE:
            if (dp->dimensions.isScalar()) {
                io->outputMessage("[handle]");
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" handle]");
            }
            break;
        case NLS_LOGICAL:
            if (!isSparse() && dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const logical*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" logical]");
            }
            break;
        case NLS_UINT8:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint8*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint8]");
            }
            break;
        case NLS_INT8:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int8*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int8]");
            }
            break;
        case NLS_UINT16:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint16*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint16]");
            }
            break;
        case NLS_INT16:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int16*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int16]");
            }
            break;
        case NLS_UINT32:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint32*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint32]");
            }
            break;
        case NLS_INT32:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int32*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int32]");
            }
            break;
        case NLS_UINT64: {
            if (dp->dimensions.isScalar()) {
                uint64 val = *((const uint64*)dp->getData());
                std::string msg = "[" + std::to_string(val) + "]";
                // snprintf(msgBuffer, MSGBUFLEN, "[" PRIu64 "]", *((const
                // uint64*)dp->getData())); io->outputMessage(msgBuffer);
                io->outputMessage(msg.c_str());
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint64]");
            }
        } break;
        case NLS_INT64: {
            if (dp->dimensions.isScalar()) {
                int64 value = *((const int64*)dp->getData());
                std::string msg = std::string("[") + std::to_string(value) + std::string("]");
                // snprintf(msgBuffer, MSGBUFLEN, "[" PRId64 "]", *((const
                // int64*)dp->getData()));
                io->outputMessage(msg.c_str());
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int64]");
            }
        } break;
        case NLS_DOUBLE:
            if (!isSparse() && dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%lf]", *((const double*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" double]");
            }
            break;
        case NLS_DCOMPLEX:
            if (!isSparse() && dp->dimensions.isScalar()) {
                const double* ap = (const double*)dp->getData();
                snprintf(msgBuffer, MSGBUFLEN, "[%lf+%lfi]", ap[0], ap[1]);
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" dcomplex]");
            }
            break;
        case NLS_SINGLE:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%f]", *((const single*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" single]");
            }
            break;
        case NLS_SCOMPLEX:
            if (dp->dimensions.isScalar()) {
                const single* ap = (const single*)dp->getData();
                snprintf(msgBuffer, MSGBUFLEN, "[%f+%fi]", ap[0], ap[1]);
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" complex]");
            }
            break;
        }
    }
}
//=============================================================================
void
emitElement(Interface* io, char* msgBuffer, const void* dp, indexType num, Class dcls)
{
    switch (dcls) {
    case NLS_STRUCT_ARRAY: {
    } break;
    case NLS_HANDLE: {
    } break;
    case NLS_INT8: {
        const int8* ap = (const int8*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "% 4d", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_UINT8: {
        const uint8* ap = (const uint8*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%3u", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_INT16: {
        const int16* ap = (const int16*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "% 6d", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_UINT16: {
        const uint16* ap = (const uint16*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%5u", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_INT32: {
        const int32* ap = (const int32*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%13d", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_UINT32: {
        const uint32* ap = (const uint32*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%12u", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_INT64: {
        const int64* ap = (const int64*)dp;
        std::string msg = std::to_string(ap[num]) + "  ";
        // snprintf(msgBuffer, MSGBUFLEN, "%13d", ap[num]);
        // io->outputMessage(msgBuffer);
        // snprintf(msgBuffer, MSGBUFLEN, "  ");
        // io->outputMessage(msgBuffer);
        io->outputMessage(msg.c_str());
        break;
    }
    case NLS_UINT64: {
        const uint64* ap = (const uint64*)dp;
        std::string msg("");
        msg = std::to_string(ap[num]) + "  ";
        // snprintf(msgBuffer, MSGBUFLEN, "%12u", ap[num]);
        // io->outputMessage(msgBuffer);
        // snprintf(msgBuffer, MSGBUFLEN, "  ");
        // io->outputMessage(msgBuffer);
        io->outputMessage(msg.c_str());
        break;
    }
    case NLS_LOGICAL: {
        const logical* ap = (const logical*)dp;
        if (ap[num] == 0) {
            snprintf(msgBuffer, MSGBUFLEN, "false  ");
        } else {
            snprintf(msgBuffer, MSGBUFLEN, "true   ");
        }
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_CHAR: {
        const charType* ap = (const charType*)dp;
        std::wstring wstr;
        wstr.push_back(ap[num]);
        io->outputMessage(wstr);
        break;
    }
    case NLS_SINGLE: {
        const single* ap = (const single*)dp;
        outputSinglePrecisionFloat(msgBuffer, ap[num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_DOUBLE: {
        const double* ap = (const double*)dp;
        outputDoublePrecisionFloat(msgBuffer, ap[num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_SCOMPLEX: {
        const single* ap = (const single*)dp;
        outputSinglePrecisionFloat(msgBuffer, ap[2 * num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, " ");
        io->outputMessage(msgBuffer);
        outputSinglePrecisionFloat(msgBuffer, ap[2 * num + 1]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "i  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_DCOMPLEX: {
        const double* ap = (const double*)dp;
        outputDoublePrecisionFloat(msgBuffer, ap[2 * num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, " ");
        io->outputMessage(msgBuffer);
        outputDoublePrecisionFloat(msgBuffer, ap[2 * num + 1]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "i  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_CELL_ARRAY: {
        ArrayOf* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            io->outputMessage("[]");
        } else {
            ap[num].summarizeCellEntry(io);
        }
        break;
    }
    case NLS_STRING_ARRAY: {
        ArrayOf* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            io->outputMessage("[]");
        } else {
            ap[num].summarizeStringArray(io);
        }
        break;
    }
    }
}
//=============================================================================
/**
 * Display this variable on the given output stream.
 */
void
ArrayOf::printMe(Interface* io) const
{
    int nominalWidth;
    sizeType termWidth = io->getTerminalWidth();
    // Print the class...
    switch (dp->dataClass) {
    case NLS_HANDLE:
        io->outputMessage("  <handle>  ");
        nominalWidth = 5;
        break;
    case NLS_UINT8:
        io->outputMessage("  <uint8>  ");
        nominalWidth = 5;
        break;
    case NLS_INT8:
        io->outputMessage("  <int8>  ");
        nominalWidth = 6;
        break;
    case NLS_UINT16:
        io->outputMessage("  <uint16>  ");
        nominalWidth = 7;
        break;
    case NLS_INT16:
        io->outputMessage("  <int16>  ");
        nominalWidth = 8;
        break;
    case NLS_UINT32:
        io->outputMessage("  <uint32>  ");
        nominalWidth = 14;
        break;
    case NLS_INT32:
        io->outputMessage("  <int32>  ");
        nominalWidth = 15;
        break;
    case NLS_UINT64:
        io->outputMessage("  <uint64>  ");
        nominalWidth = 14;
        break;
    case NLS_INT64:
        io->outputMessage("  <int64>  ");
        nominalWidth = 15;
        break;
    case NLS_SINGLE:
        io->outputMessage("  <single>  ");
        nominalWidth = 20;
        break;
    case NLS_DOUBLE:
        io->outputMessage("  <double>  ");
        nominalWidth = 30;
        break;
    case NLS_LOGICAL:
        io->outputMessage("  <logical>  ");
        nominalWidth = 2;
        break;
    case NLS_CHAR:
        io->outputMessage("  <char>  ");
        nominalWidth = 1;
        break;
    case NLS_SCOMPLEX:
        io->outputMessage("  <single>  ");
        nominalWidth = 36;
        break;
    case NLS_DCOMPLEX:
        io->outputMessage("  <double>  ");
        nominalWidth = 54;
        break;
    case NLS_CELL_ARRAY:
        io->outputMessage("  <cell> ");
        nominalWidth = 10;
        break;
    case NLS_STRUCT_ARRAY:
        io->outputMessage("  <struct> ");
        nominalWidth = 10;
        break;
    case NLS_STRING_ARRAY:
        io->outputMessage("  <string> ");
        nominalWidth = 10;
        break;
    }
    io->outputMessage("- size: ");
    dp->dimensions.printMe(io);
    io->outputMessage("\n");
    if (isEmpty()) {
        if (isStruct()) {
            stringVector fieldsName = getFieldNames();
            if (fieldsName.size() == 0) {
                io->outputMessage("  []\n");
            } else {
                for (size_t k = 0; k < fieldsName.size(); k++) {
                    io->outputMessage("    ");
                    io->outputMessage(fieldsName[k]);
                    io->outputMessage("\n");
                }
            }
        } else {
            io->outputMessage("  []\n");
        }
        return;
    }
    if (isSparse()) {
        sprintf(msgBuffer, _("\tMatrix is sparse with %d nonzeros\n").c_str(), getNonzeros());
        io->outputMessage(msgBuffer);
        return;
    }
    if (dp->dataClass == NLS_STRUCT_ARRAY) {
        if (dp->dimensions.isScalar()) {
            ArrayOf* ap;
            ap = (ArrayOf*)dp->getData();
            for (sizeType n = 0; n < (sizeType)dp->fieldNames.size(); n++) {
                io->outputMessage("    ");
                io->outputMessage(dp->fieldNames[n].c_str());
                io->outputMessage(": ");
                ap[n].summarizeCellEntry(io);
                io->outputMessage("\n");
            }
        } else {
            if (dp->fieldNames.size() > 0) {
                io->outputMessage("  Fields\n");
                for (sizeType n = 0; n < (sizeType)dp->fieldNames.size(); n++) {
                    io->outputMessage("    ");
                    io->outputMessage(dp->fieldNames[n].c_str());
                    io->outputMessage("\n");
                }
            }
        }
    } else {
        const void* ap = dp->getData();
        if (dp->dimensions.getLength() == 2) {
            indexType rows = dp->dimensions.getRows();
            indexType columns = dp->dimensions.getColumns();
            int items_printed;
            items_printed = 0;
            // Determine how many columns will fit across
            // the terminal width
            indexType colsPerPage = (indexType)floor((termWidth - 1) / ((single)nominalWidth));
            indexType pageCount = (indexType)ceil(columns / ((single)colsPerPage));
            for (indexType k = 0; k < pageCount; k++) {
                indexType colsInThisPage = columns - colsPerPage * k;
                colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                if (dp->dimensions.getElementCount() > 1 && dp->dataClass != NLS_CHAR) {
                    snprintf(msgBuffer, MSGBUFLEN, _("\nColumns %d to %d\n").c_str(),
                        k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                    io->outputMessage(msgBuffer);
                }
                memset(msgBuffer, 0, MSGBUFLEN);
                for (indexType i = 0; i < rows; i++) {
                    if (dp->dataClass == NLS_CHAR) {
                        snprintf(msgBuffer, MSGBUFLEN, " '");
                    } else {
                        snprintf(msgBuffer, MSGBUFLEN, " ");
                    }
                    io->outputMessage(msgBuffer);
                    memset(msgBuffer, 0, MSGBUFLEN);
                    for (indexType j = 0; j < colsInThisPage; j++) {
                        emitElement(
                            io, msgBuffer, ap, i + (k * colsPerPage + j) * rows, dp->dataClass);
                        if ((j < colsInThisPage - 1) && dp->dataClass != NLS_CHAR) {
                            io->outputMessage(" ");
                        }
                        items_printed++;
                    }
                    if (dp->dataClass == NLS_CHAR) {
                        snprintf(msgBuffer, MSGBUFLEN, "'\n");
                    } else {
                        snprintf(msgBuffer, MSGBUFLEN, "\n");
                    }
                    io->outputMessage(msgBuffer);
                    memset(msgBuffer, 0, MSGBUFLEN);
                }
            }
        } else if (dp->dimensions.getLength() > 2) {
            /**
             * For N-ary arrays, data slice  -  start with
             * [1,1,1,...,1].  We keep doing the matrix
             * print , incrementing from the highest dimension,
             * and rolling downwards.
             */
            Dimensions wdims(dp->dimensions.getLength());
            indexType rows(dp->dimensions.getRows());
            indexType columns(dp->dimensions.getColumns());
            int items_printed;
            items_printed = 0;
            indexType offset = 0;
            while (wdims.inside(dp->dimensions)) {
                snprintf(msgBuffer, MSGBUFLEN, "(:,:");
                io->outputMessage(msgBuffer);
                for (sizeType m = 2; m < dp->dimensions.getLength(); m++) {
                    snprintf(msgBuffer, MSGBUFLEN, ",%d", (int)wdims[m] + 1);
                    io->outputMessage(msgBuffer);
                }
                snprintf(msgBuffer, MSGBUFLEN, ") =\n\n");
                io->outputMessage(msgBuffer);
                // Determine how many columns will fit across
                // the terminal width
                indexType colsPerPage = (indexType)floor((termWidth - 1) / ((single)nominalWidth));
                int pageCount;
                pageCount = (int)ceil(columns / ((single)colsPerPage));
                for (int k = 0; k < pageCount; k++) {
                    indexType colsInThisPage = columns - colsPerPage * k;
                    colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                    snprintf(msgBuffer, MSGBUFLEN, _("\nColumns %d to %d\n").c_str(),
                        k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                    io->outputMessage(msgBuffer);
                    memset(msgBuffer, 0, MSGBUFLEN);
                    for (indexType i = 0; i < rows; i++) {
                        snprintf(msgBuffer, MSGBUFLEN, " ");
                        io->outputMessage(msgBuffer);
                        memset(msgBuffer, 0, MSGBUFLEN);
                        if (dp->dataClass == NLS_CHAR) {
                            io->outputMessage("'");
                        }
                        for (indexType j = 0; j < colsInThisPage; j++) {
                            emitElement(io, msgBuffer, ap,
                                i + (k * colsPerPage + j) * rows + offset, dp->dataClass);
                            items_printed++;
                        }
                        if (dp->dataClass == NLS_CHAR) {
                            io->outputMessage("'");
                        }
                        snprintf(msgBuffer, MSGBUFLEN, "\n");
                        io->outputMessage(msgBuffer);
                        memset(msgBuffer, 0, MSGBUFLEN);
                    }
                }
                offset += rows * columns;
                wdims.incrementModulo(dp->dimensions, 2);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
