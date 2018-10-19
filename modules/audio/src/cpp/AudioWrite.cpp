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
#include "AudioWrite.hpp"
#include "ComplexTranspose.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <sndfile.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
extensionToFormat(std::wstring extension, int& format)
{
    int countFormat = 0;
    std::wstring EXT = boost::to_upper_copy(extension);
    sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT, &countFormat, sizeof(int));
    for (int i = 0; i < countFormat; i++) {
        SF_FORMAT_INFO info;
        info.format = i;
        sf_command(NULL, SFC_GET_FORMAT_MAJOR, &info, sizeof(info));
        std::wstring ext = L"." + utf8_to_wstring(info.extension);
        boost::to_upper(ext);
        if (ext == EXT) {
            format = info.format;
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
convertBitsPerSample(SF_INFO& sfinfo, int BitsPerSample)
{
    sfinfo.format &= ~SF_FORMAT_SUBMASK;
    if (BitsPerSample == 8) {
        if ((sfinfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV) {
            sfinfo.format |= SF_FORMAT_PCM_U8;
            return true;
        } else {
            sfinfo.format |= SF_FORMAT_PCM_S8;
            return true;
        }
    } else if (BitsPerSample == 16) {
        sfinfo.format |= SF_FORMAT_PCM_16;
        return true;
    } else if (BitsPerSample == 24) {
        sfinfo.format |= SF_FORMAT_PCM_24;
        return true;
    } else if (BitsPerSample == 32) {
        sfinfo.format |= SF_FORMAT_PCM_32;
        return true;
    }
    return false;
}
//=============================================================================
bool
isSupportedDataType(ArrayOf data)
{
    Class dataClass = data.getDataClass();
    return (dataClass == NLS_UINT8) || (dataClass == NLS_INT16) || (dataClass == NLS_INT32)
        || (dataClass == NLS_SINGLE) || (dataClass == NLS_DOUBLE);
}
//=============================================================================
bool
AudioWrite(std::wstring filename, ArrayOf data, int fs, wstringVector metadata, int BitsPerSample,
    int BitRate, std::wstring& errorMessage)
{
    if (!data.isNumeric() || data.isComplex() || data.isSparse()) {
        errorMessage = ERROR_WRONG_ARGUMENT_1_TYPE;
        return false;
    }
    if (!isSupportedDataType(data)) {
        errorMessage = ERROR_WRONG_ARGUMENT_1_TYPE;
        return false;
    }
    if (data.isEmpty()) {
        errorMessage = _W("Empty matrix not allowed.");
        return false;
    }
    if (!data.is2D() && !data.isVector()) {
        errorMessage = _W("Vector or matrix 2D expected.");
        return false;
    }
    indexType rows = data.getDimensions().getRows();
    indexType columns = data.getDimensions().getColumns();
    ArrayOf audioData;
    if (columns > rows) {
        bool needToOverload;
        audioData = ComplexTranspose(data, needToOverload);
    } else {
        audioData = data;
    }
    rows = audioData.getDimensions().getRows();
    columns = audioData.getDimensions().getColumns();
    int nbChannels = (int)columns;
    boost::filesystem::path pathFilename = filename;
    std::wstring extension;
    if (pathFilename.has_extension()) {
        extension = pathFilename.extension().generic_wstring();
        int format;
        if (!extensionToFormat(extension, format)) {
            errorMessage = _W("Invalid file extension.");
            return false;
        }
        SF_INFO sfinfo;
        memset(&sfinfo, 0, sizeof(sfinfo));
        sfinfo.format = format;
        sfinfo.channels = nbChannels;
        sfinfo.samplerate = fs;
        if (!convertBitsPerSample(sfinfo, BitsPerSample)) {
            errorMessage = _W("Invalid BitsPerSample value.");
            return false;
        }
        SNDFILE* file = nullptr;
#ifdef _MSC_VER
        file = sf_wchar_open(filename.c_str(), SFM_WRITE, &sfinfo);
#else
        std::string ufilename = wstring_to_utf8(filename);
        file = sf_open(ufilename.c_str(), SFM_WRITE, &sfinfo);
#endif
        if (file == nullptr) {
            const char* msg = sf_strerror(NULL);
            errorMessage = utf8_to_wstring(msg);
            return false;
        }
        int BUFFER = 8192;
        int writecount = 0;
        long total = 0;
        int idx = 0;
        switch (audioData.getDataClass()) {
        case NLS_UINT8: {
            uint8* ptrAudioData = (uint8*)audioData.getDataPointer();
            single* buffer
                = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, BUFFER * sfinfo.channels);
            int idx = 0;
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = (int)(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        single value
                            = (single)((ptrAudioData[idx++] - std::pow(2.0, 7)) / std::pow(2.0, 7));
                        single max_val = (value < single(-1.0)) ? single(-1.0) : value;
                        single min_val = (single(1.0) < max_val) ? single(1.0) : max_val;
                        buffer[k * sfinfo.channels + ch] = min_val;
                    }
                }
                if (writecount > 0) {
                    sf_count_t audio_data_written = sf_writef_float(file, buffer, writecount);
                    if (audio_data_written != writecount) {
                        delete[] buffer;
                        errorMessage = _W("Write failed.");
                        sf_close(file);
                        return false;
                    }
                }
                total += writecount;
            } while (writecount > 0 && total < rows);
            delete[] buffer;
        } break;
        case NLS_INT16: {
            int16* ptrAudioData = (int16*)audioData.getDataPointer();
            int16* buffer = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, BUFFER * sfinfo.channels);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = (int)(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[k * sfinfo.channels + ch] = ptrAudioData[idx++];
                    }
                }
                if (writecount > 0) {
                    sf_count_t audio_data_written = sf_writef_short(file, buffer, writecount);
                    if (audio_data_written != writecount) {
                        delete[] buffer;
                        errorMessage = _W("Write failed.");
                        sf_close(file);
                        return false;
                    }
                }
                total += writecount;
            } while (writecount > 0 && total < rows);
            delete[] buffer;
        } break;
        case NLS_INT32: {
            int32* ptrAudioData = (int32*)audioData.getDataPointer();
            int32* buffer = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, BUFFER * sfinfo.channels);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = (int)(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[k * sfinfo.channels + ch] = ptrAudioData[idx++];
                    }
                }
                if (writecount > 0) {
                    sf_count_t audio_data_written = sf_writef_int(file, buffer, writecount);
                    if (audio_data_written != writecount) {
                        delete[] buffer;
                        errorMessage = _W("Write failed.");
                        sf_close(file);
                        return false;
                    }
                }
                total += writecount;
            } while (writecount > 0 && total < rows);
            delete[] buffer;
        } break;
        case NLS_SINGLE: {
            single* ptrAudioData = (single*)audioData.getDataPointer();
            single* buffer
                = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, BUFFER * sfinfo.channels);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = (int)(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[k * sfinfo.channels + ch] = ptrAudioData[idx++];
                    }
                }
                if (writecount > 0) {
                    sf_count_t audio_data_written = sf_writef_float(file, buffer, writecount);
                    if (audio_data_written != writecount) {
                        delete[] buffer;
                        errorMessage = _W("Write failed.");
                        sf_close(file);
                        return false;
                    }
                }
                total += writecount;
            } while (writecount > 0 && total < rows);
            delete[] buffer;
        } break;
        case NLS_DOUBLE: {
            double* ptrAudioData = (double*)audioData.getDataPointer();
            double* buffer
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, BUFFER * sfinfo.channels);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = (int)(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[k * sfinfo.channels + ch] = ptrAudioData[idx++];
                    }
                }
                if (writecount > 0) {
                    sf_count_t audio_data_written = sf_writef_double(file, buffer, writecount);
                    if (audio_data_written != writecount) {
                        delete[] buffer;
                        errorMessage = _W("Write failed.");
                        sf_close(file);
                        return false;
                    }
                }
                total += writecount;
            } while (writecount > 0 && total < rows);
            delete[] buffer;
        } break;
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
    } else {
        errorMessage = _W("Filename must have an extension.");
    }
    return false;
}
//=============================================================================
}