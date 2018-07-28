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
#ifdef _MSC_VER
#include <Windows.h>
#define ENABLE_SNDFILE_WINDOWS_PROTOTYPES 1
#endif
//=============================================================================
#include "AudioRead.hpp"
#include "characters_encoding.hpp"
#include <Eigen/Dense>
#include <climits>
#include <sndfile.h>
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_FRAMES 8192
//=============================================================================
ArrayOfVector
AudioRead(std::wstring filename, double dstart, double dend, std::wstring datatype,
    std::wstring& errorMessage)
{
    ArrayOfVector retval;
    SNDFILE* file = nullptr;
    if (datatype != L"native" && datatype != L"double") {
        errorMessage = _W("'native' or 'double' expected.");
        return retval;
    }
    SF_INFO sfinfo;
    memset(&sfinfo, 0, sizeof(sfinfo));
#ifdef _MSC_VER
    file = sf_wchar_open(filename.c_str(), SFM_READ, &sfinfo);
#else
    std::string ufilename = wstring_to_utf8(filename);
    file = sf_open(ufilename.c_str(), SFM_READ, &sfinfo);
#endif
    if (file == nullptr) {
        errorMessage = ERROR_WRONG_ARGUMENT_1_VALUE;
        return retval;
    }
    if (dstart < 1 || dstart > dend) {
        sf_close(file);
        errorMessage = _W("Invalid range.");
        return retval;
    }
    indexType start = (indexType)dstart;
    indexType end = (indexType)dend + 1;
    ArrayOfVector index;
    if (std::isinf(dend)) {
        end = (indexType)sfinfo.frames;
    }
    bool allFrames = true;
    if (start == 1 && end == sfinfo.frames) {
        allFrames = true;
    } else {
        ArrayOf rangeIndex;
#ifdef NLS_INDEX_TYPE_64
        int64 n = (int64)(end - start);
        Dimensions dims(1, n);
        int64* pV = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, n);
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, 1>> Range(pV, n);
        Range = Eigen::Matrix<int64, Eigen::Dynamic, 1>::LinSpaced(n, start, end);
        rangeIndex = ArrayOf(NLS_INT64, dims, pV);
#else
        int32 n = (int32)(end - start);
        Dimensions dims(1, n);
        int32* pV = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, n);
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, 1>> Range(pV, n);
        Range = Eigen::Matrix<int32, Eigen::Dynamic, 1>::LinSpaced(n, start, end);
        rangeIndex = ArrayOf(NLS_INT32, dims, pV);
#endif
        index.push_back(rangeIndex);
        index.push_back(ArrayOf::characterArrayConstructor(":"));
        allFrames = false;
    }
    if (sf_command(file, SFC_SET_NORM_DOUBLE, NULL, SF_TRUE) == SF_FALSE) {
        errorMessage = _W("Cannot read audio file.");
        return retval;
    }
    sf_count_t readcount = 0;
    sf_count_t total = 0;
    indexType n_out = (indexType)sfinfo.channels;
    indexType m_out = (indexType)sfinfo.frames;
    Dimensions dims(m_out, n_out);
    int format = SF_FORMAT_DOUBLE;
    if (datatype == L"double") {
        format = SF_FORMAT_DOUBLE;
    } else {
        format = sfinfo.format & SF_FORMAT_SUBMASK;
    }
    ArrayOf y;
    switch (format) {
    case SF_FORMAT_PCM_S8: {
        single* dataAsSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dims.getElementCount());
        readcount = sf_read_float(file, dataAsSingle, dims.getElementCount());
        sf_close(file);
        if (readcount == sfinfo.frames * sfinfo.channels) {
            int8* dataAsInt8 = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, dims.getElementCount());
            for (indexType k = 0; k < dims.getElementCount(); k++) {
                for (indexType i = 0; i < n_out; ++i) {
                    for (indexType j = 0; j < m_out; ++j) {
                        dataAsInt8[i * m_out + j] = (uint8)(dataAsSingle[j * n_out + i] * 127);
                    }
                }
            }
            delete[] dataAsSingle;
            y = ArrayOf(NLS_INT8, dims, dataAsInt8);
        } else {
            delete[] dataAsSingle;
            errorMessage = _W("Cannot read audio file.");
        }
    } break;
    case SF_FORMAT_PCM_U8: {
        single* dataAsSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dims.getElementCount());
        readcount = sf_read_float(file, dataAsSingle, dims.getElementCount());
        sf_close(file);
        if (readcount == sfinfo.frames * sfinfo.channels) {
            uint8* dataAsUInt8
                = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dims.getElementCount());
            for (size_t k = 0; k < (size_t)dims.getElementCount(); k++) {
                for (indexType i = 0; i < n_out; ++i) {
                    for (indexType j = 0; j < m_out; ++j) {
                        dataAsUInt8[i * m_out + j]
                            = (uint8)(dataAsSingle[j * n_out + i] * 127 + 127);
                    }
                }
            }
            delete[] dataAsSingle;
            y = ArrayOf(NLS_UINT8, dims, dataAsUInt8);
        } else {
            delete[] dataAsSingle;
            errorMessage = _W("Cannot read audio file.");
        }
    } break;
    case SF_FORMAT_PCM_16: {
        int16* dataAsInt16RowMajor
            = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, dims.getElementCount());
        readcount = sf_read_short(file, dataAsInt16RowMajor, dims.getElementCount());
        sf_close(file);
        if (readcount == sfinfo.frames * sfinfo.channels) {
            int16* dataAsInt16ColumnMajor
                = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, dims.getElementCount());
            for (indexType i = 0; i < n_out; ++i) {
                for (indexType j = 0; j < m_out; ++j) {
                    dataAsInt16ColumnMajor[i * m_out + j] = dataAsInt16RowMajor[j * n_out + i];
                }
            }
            delete[] dataAsInt16RowMajor;
            y = ArrayOf(NLS_INT16, dims, dataAsInt16ColumnMajor);
        } else {
            delete[] dataAsInt16RowMajor;
            errorMessage = _W("Cannot read audio file.");
        }
    } break;
    case SF_FORMAT_PCM_24:
    case SF_FORMAT_PCM_32: {
        int32* dataAsInt32RowMajor
            = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, dims.getElementCount());
        readcount = sf_readf_int(file, dataAsInt32RowMajor, BUFFER_FRAMES);
        sf_close(file);
        if (readcount == sfinfo.frames * sfinfo.channels) {
            int32* dataAsInt32ColumnMajor
                = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, dims.getElementCount());
            for (indexType i = 0; i < n_out; ++i) {
                for (indexType j = 0; j < m_out; ++j) {
                    dataAsInt32ColumnMajor[i * m_out + j] = dataAsInt32RowMajor[j * n_out + i];
                }
            }
            delete[] dataAsInt32RowMajor;
            y = ArrayOf(NLS_INT32, dims, dataAsInt32ColumnMajor);
        } else {
            delete[] dataAsInt32RowMajor;
            errorMessage = _W("Cannot read audio file.");
        }
    } break;
    case SF_FORMAT_FLOAT: {
        single* dataAsSingleRowMajor
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dims.getElementCount());
        readcount = sf_read_float(file, dataAsSingleRowMajor, dims.getElementCount());
        sf_close(file);
        if (readcount == sfinfo.frames * sfinfo.channels) {
            single* dataAsSingleColumnMajor
                = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dims.getElementCount());
            for (indexType i = 0; i < n_out; ++i) {
                for (indexType j = 0; j < m_out; ++j) {
                    dataAsSingleColumnMajor[i * m_out + j] = dataAsSingleRowMajor[j * n_out + i];
                }
            }
            delete[] dataAsSingleRowMajor;
            y = ArrayOf(NLS_SINGLE, dims, dataAsSingleColumnMajor);
        } else {
            delete[] dataAsSingleRowMajor;
            errorMessage = _W("Cannot read audio file.");
        }
    } break;
    default: // double
    {
        double* dataAsDoubleRowMajor
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount());
        readcount = sf_read_double(file, dataAsDoubleRowMajor, dims.getElementCount());
        sf_close(file);
        if (readcount == sfinfo.frames * sfinfo.channels) {
            double* dataAsDoubleColumnMajor
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount());
            for (indexType i = 0; i < n_out; ++i) {
                for (indexType j = 0; j < m_out; ++j) {
                    dataAsDoubleColumnMajor[i * m_out + j] = dataAsDoubleRowMajor[j * n_out + i];
                }
            }
            delete[] dataAsDoubleRowMajor;
            y = ArrayOf(NLS_DOUBLE, dims, dataAsDoubleColumnMajor);
        } else {
            delete[] dataAsDoubleRowMajor;
            errorMessage = _W("Cannot read audio file.");
        }
    } break;
    }
    if (errorMessage == L"") {
        if (!allFrames) {
            y = y.getNDimSubset(index);
        }
        retval.push_back(y);
        retval.push_back(ArrayOf::doubleConstructor(sfinfo.samplerate));
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
