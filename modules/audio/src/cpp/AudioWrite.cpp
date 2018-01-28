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
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <sndfile.h>
#include "AudioWrite.hpp"
#include "characters_encoding.hpp"
#include "ComplexTranspose.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static bool extensionToFormat(std::wstring extension, int &format)
    {
        int countFormat = 0;
        std::wstring EXT = boost::to_upper_copy(extension);
        sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT, &countFormat, sizeof(int));
        for (int i = 0; i < countFormat; i++)
        {
            SF_FORMAT_INFO info;
            info.format = i;
            sf_command(NULL, SFC_GET_FORMAT_MAJOR, &info, sizeof(info));
            std::wstring ext = L"." + utf8_to_wstring(info.extension);
            boost::to_upper(ext);
            if (ext == EXT)
            {
                format = info.format;
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    static bool convertBitsPerSample(SF_INFO &sfinfo, int BitsPerSample)
    {
        sfinfo.format &= ~SF_FORMAT_SUBMASK;
        if (BitsPerSample == 8)
        {
            if ((sfinfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)
            {
                sfinfo.format |= SF_FORMAT_PCM_U8;
                return true;
            }
            else
            {
                sfinfo.format |= SF_FORMAT_PCM_S8;
                return true;
            }
        }
        else if (BitsPerSample == 16)
        {
            sfinfo.format |= SF_FORMAT_PCM_16;
            return true;
        }
        else if (BitsPerSample == 24)
        {
            sfinfo.format |= SF_FORMAT_PCM_24;
            return true;
        }
        else if (BitsPerSample == 32)
        {
            sfinfo.format |= SF_FORMAT_PCM_32;
            return true;
        }
        return false;
    }
    //=============================================================================
    bool isSupportedDataType(ArrayOf data)
    {
        Class dataClass = data.getDataClass();
        return (dataClass == NLS_UINT8) ||
               (dataClass == NLS_INT16) ||
               (dataClass == NLS_INT32) ||
               (dataClass == NLS_SINGLE) ||
               (dataClass == NLS_DOUBLE);
    }
    //=============================================================================
    bool AudioWrite(std::wstring filename, ArrayOf data, int fs, wstringVector metadata, int BitsPerSample, int BitRate, std::wstring &errorMessage)
    {
        if (!data.isNumeric() || data.isComplex() || data.isSparse())
        {
            errorMessage = ERROR_WRONG_ARGUMENT_1_TYPE;
            return false;
        }
        if (!isSupportedDataType(data))
        {
            errorMessage = ERROR_WRONG_ARGUMENT_1_TYPE;
            return false;
        }
        if (data.isEmpty())
        {
            errorMessage = _W("Empty matrix not allowed.");
            return false;
        }
        if (!data.is2D() && !data.isVector())
        {
            errorMessage = _W("Vector or matrix 2D expected.");
            return false;
        }
        indexType rows = data.getDimensions().getRows();
        indexType columns = data.getDimensions().getColumns();
        ArrayOf audioData;
        if (columns > rows)
        {
            audioData = ComplexTranspose(data);
        }
        else
        {
            audioData = data;
        }
        rows = audioData.getDimensions().getRows();
        columns = audioData.getDimensions().getColumns();
        int nbChannels = columns;
        boost::filesystem::path pathFilename = filename;
        std::wstring extension;
        if (pathFilename.has_extension())
        {
            extension = pathFilename.extension().generic_wstring();
            int format;
            if (!extensionToFormat(extension, format))
            {
                errorMessage = _W("Invalid file extension.");
                return false;
            }
            SF_INFO sfinfo;
            memset(&sfinfo, 0, sizeof(sfinfo));
            sfinfo.format = format;
            sfinfo.channels = nbChannels;
            sfinfo.samplerate = fs;
            if (!convertBitsPerSample(sfinfo, BitsPerSample))
            {
                errorMessage = _W("Invalid BitsPerSample value.");
                return false;
            }
            int16 *audioAsInt16RowMajor = nullptr;
            int32 *audioAsInt32RowMajor = nullptr;
            single *audioAsSingleRowMajor = nullptr;
            double *audioAsDoubleRowMajor = nullptr;
            switch (audioData.getDataClass())
            {
                case NLS_UINT8:
                {
                    audioAsSingleRowMajor = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, audioData.getDimensions().getElementCount());
                    uint8 *ptrAudioData = (uint8*)audioData.getDataPointer();
                    int index = 0;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < columns; j++)
                        {
                            single value = (single)((ptrAudioData[j * columns + i] - std::pow(2.0, 7)) / std::pow(2.0, 7));
                            single max_val = (value < single(-1.0)) ? single(-1.0) : value;
                            single min_val = (single(1.0) < max_val) ? single(1.0) : max_val;
                            audioAsSingleRowMajor[index++] = min_val;
                        }
                    }
                }
                break;
                case NLS_INT16:
                {
                    audioAsInt16RowMajor = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, audioData.getDimensions().getElementCount());
                    int16 *ptrAudioData = (int16*)audioData.getDataPointer();
                    int index = 0;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < columns; j++)
                        {
                            audioAsInt16RowMajor[index++] = ptrAudioData[j * columns + i];
                        }
                    }
                }
                break;
                case NLS_INT32:
                {
                    int32 *audioAsInt32RowMajor = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, audioData.getDimensions().getElementCount());
                    int32 *ptrAudioData = (int32*)audioData.getDataPointer();
                    int index = 0;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < columns; j++)
                        {
                            audioAsInt32RowMajor[index++] = ptrAudioData[j * columns + i];
                        }
                    }
                }
                break;
                case NLS_SINGLE:
                {
                    audioAsSingleRowMajor = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, audioData.getDimensions().getElementCount());
                    single *ptrAudioData = (single*)audioData.getDataPointer();
                    int index = 0;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < columns; j++)
                        {
                            audioAsSingleRowMajor[index++] = ptrAudioData[j * columns + i];
                        }
                    }
                }
                break;
                case NLS_DOUBLE:
                {
                    audioAsDoubleRowMajor = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, audioData.getDimensions().getElementCount());
                    double *ptrAudioData = (double*)audioData.getDataPointer();
                    int index = 0;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < columns; j++)
                        {
                            audioAsDoubleRowMajor[index++] = ptrAudioData[j * columns + i];
                        }
                    }
                }
                break;
            }
            SNDFILE * file = nullptr;
#ifdef _MSC_VER
            file = sf_wchar_open(filename.c_str(), SFM_WRITE, &sfinfo);
#else
            std::string ufilename = wstring_to_utf8(filename);
            file = sf_open(ufilename.c_str(), SFM_WRITE, &sfinfo);
#endif
            if (file == nullptr)
            {
                if (audioAsInt16RowMajor)
                {
                    delete[] audioAsInt16RowMajor;
                }
                if (audioAsInt32RowMajor)
                {
                    delete[] audioAsInt32RowMajor;
                }
                if (audioAsSingleRowMajor)
                {
                    delete[] audioAsSingleRowMajor;
                }
                if (audioAsDoubleRowMajor)
                {
                    delete[] audioAsDoubleRowMajor;
                }
                const char* msg = sf_strerror(NULL);
                errorMessage = utf8_to_wstring(msg);
                return false;
            }
            sf_count_t total_audio_data = 0;
            sf_count_t offset = 0;
            sf_count_t audio_data_to_write = rows * columns;
            sf_count_t chunk_size = audio_data_to_write;
            while (total_audio_data < audio_data_to_write)
            {
                if (audio_data_to_write - offset < chunk_size)
                {
                    chunk_size = audio_data_to_write - offset;
                }
                sf_count_t audio_data_written;
                switch (audioData.getDataClass())
                {
                    case NLS_UINT8:
                    {
                        audio_data_written = sf_write_float(file, audioAsSingleRowMajor + offset, chunk_size);
                    }
                    break;
                    case NLS_INT16:
                    {
                        audio_data_written = sf_write_short(file, audioAsInt16RowMajor + offset, chunk_size);
                    }
                    break;
                    case NLS_INT32:
                    {
                        audio_data_written = sf_write_int(file, audioAsInt32RowMajor + offset, chunk_size);
                    }
                    break;
                    case NLS_SINGLE:
                    {
                        audio_data_written = sf_write_float(file, audioAsSingleRowMajor + offset, chunk_size);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        audio_data_written = sf_write_double(file, audioAsDoubleRowMajor + offset, chunk_size);
                    }
                    break;
                }
                if (audio_data_written != chunk_size)
                {
                    if (audioAsInt16RowMajor)
                    {
                        delete[] audioAsInt16RowMajor;
                    }
                    if (audioAsInt32RowMajor)
                    {
                        delete[] audioAsInt32RowMajor;
                    }
                    if (audioAsSingleRowMajor)
                    {
                        delete[] audioAsSingleRowMajor;
                    }
                    if (audioAsDoubleRowMajor)
                    {
                        delete[] audioAsDoubleRowMajor;
                    }
                    errorMessage = _W("Write failed.");
                    sf_close(file);
                    return false;
                }
                total_audio_data += audio_data_written;
                offset += chunk_size;
            }
            if (audioAsInt16RowMajor)
            {
                delete[] audioAsInt16RowMajor;
            }
            if (audioAsInt32RowMajor)
            {
                delete[] audioAsInt32RowMajor;
            }
            if (audioAsSingleRowMajor)
            {
                delete[] audioAsSingleRowMajor;
            }
            if (audioAsDoubleRowMajor)
            {
                delete[] audioAsDoubleRowMajor;
            }
            // metadata works only for wav
            std::string title = wstring_to_utf8(metadata[0]);
            sf_set_string(file, SF_STR_TITLE, title.c_str());
            std::string artist = wstring_to_utf8(metadata[1]);
            sf_set_string(file, SF_STR_ARTIST, artist.c_str());
            std::string comment = wstring_to_utf8(metadata[2]);
            sf_set_string(file, SF_STR_COMMENT, comment.c_str());
            sf_close(file);
            return true;
        }
        else
        {
            errorMessage = _W("Filename must have an extension.");
        }
        return false;
    }
    //=============================================================================
}