//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <sndfile.h>
#include <map>
#include <cstring>
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "AudioWrite.hpp"
#include "ComplexTranspose.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
extensionToFormat(const std::wstring& extension, int& format)
{
#define SF_FORMAT_MPEG_LAYER_III 0x0082
#define SF_FORMAT_MPEG 0x230000
#define SF_FORMAT_OPUS 0x0064

    static bool tableFormatInitialized = false;
    static std::map<std::wstring, int> tableFormat;
    if (!tableFormatInitialized) {
        int majorCount = 0;
        sf_command(nullptr, SFC_GET_FORMAT_MAJOR_COUNT, &majorCount, sizeof(int));
        bool haveOgg = false;
        bool haveFlac = false;
        bool haveMpg = false;
        bool haveCaf = false;
        for (int m = 0; m < majorCount; m++) {
            SF_FORMAT_INFO info;
            info.format = m;
            sf_command(nullptr, SFC_GET_FORMAT_MAJOR, &info, sizeof(info));
            haveFlac = info.format == SF_FORMAT_FLAC ? true : haveFlac;
            haveOgg = info.format == SF_FORMAT_OGG ? true : haveOgg;
            haveMpg = info.format == SF_FORMAT_MPEG ? true : haveMpg;
            haveCaf = info.format == SF_FORMAT_CAF ? true : haveCaf;
        };

        tableFormat[L".wav"] = SF_FORMAT_WAV;
        tableFormat[L".aiff"] = SF_FORMAT_AIFF;
        tableFormat[L".au"] = SF_FORMAT_AU;
        tableFormat[L".raw"] = SF_FORMAT_RAW;
        tableFormat[L".paf"] = SF_FORMAT_PAF;
        tableFormat[L".pvf"] = SF_FORMAT_PVF;
        tableFormat[L".svx"] = SF_FORMAT_SVX;
        tableFormat[L".iff"] = SF_FORMAT_SVX;
        tableFormat[L".nist"] = SF_FORMAT_NIST;
        tableFormat[L".voc"] = SF_FORMAT_VOC;
        tableFormat[L".ircam"] = SF_FORMAT_IRCAM;
        tableFormat[L".sf"] = SF_FORMAT_IRCAM;
        tableFormat[L".w64"] = SF_FORMAT_W64;
        tableFormat[L".mat4"] = SF_FORMAT_MAT4;
        tableFormat[L".mat5"] = SF_FORMAT_MAT5;
        tableFormat[L".htk"] = SF_FORMAT_HTK;
        tableFormat[L".sds"] = SF_FORMAT_SDS;
        tableFormat[L".avr"] = SF_FORMAT_AVR;
        tableFormat[L".wavex"] = SF_FORMAT_WAVEX;
        tableFormat[L".sd2"] = SF_FORMAT_SD2;
        if (haveFlac) {
            tableFormat[L".flac"] = SF_FORMAT_FLAC;
        }
        if (haveCaf) {
            tableFormat[L".caf"] = SF_FORMAT_CAF;
        }
        tableFormat[L".wve"] = SF_FORMAT_WVE;
        if (haveOgg) {
            tableFormat[L".ogg"] = SF_FORMAT_OGG | (SF_FORMAT_VORBIS & SF_FORMAT_SUBMASK);
            tableFormat[L".oga"] = SF_FORMAT_OGG | (SF_FORMAT_VORBIS & SF_FORMAT_SUBMASK);
        }
        tableFormat[L".mpc2k"] = SF_FORMAT_MPC2K;
        tableFormat[L".rf64"] = SF_FORMAT_RF64;
        tableFormat[L".opus"] = SF_FORMAT_OGG | SF_FORMAT_OPUS;
        if (haveMpg) {
            tableFormat[L".mp3"] = SF_FORMAT_MPEG | SF_FORMAT_MPEG_LAYER_III;
        }
        tableFormat[L".vox"] = SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM;
        tableFormat[L".aifc"] = SF_FORMAT_AIFF | SF_FORMAT_FLOAT;
        tableFormat[L".mpc"] = SF_FORMAT_MPC2K;
        tableFormat[L".xi"] = SF_FORMAT_XI;
        tableFormat[L".mat"] = SF_FORMAT_MAT5;
        tableFormat[L".mat4"] = SF_FORMAT_MAT4;

        tableFormatInitialized = true;
    }
    std::wstring EXT = StringHelpers::to_lower_copy(extension);
    std::map<std::wstring, int>::const_iterator it = tableFormat.find(EXT);
    if (it != tableFormat.end()) {
        format = it->second;
        return true;
    }
    return false;
}
//=============================================================================
static bool
convertBitsPerSample(SF_INFO& sfinfo, int BitsPerSample)
{
    if ((sfinfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_CAF) {
        if (BitsPerSample == 16) {
            sfinfo.format |= SF_FORMAT_ALAC_16;
            return true;
        }
        if (BitsPerSample == 20) {
            sfinfo.format |= SF_FORMAT_ALAC_20;
            return true;
        }
        if (BitsPerSample == 24) {
            sfinfo.format |= SF_FORMAT_ALAC_24;
            return true;
        }
        if (BitsPerSample == 32) {
            sfinfo.format |= SF_FORMAT_ALAC_32;
            return true;
        }
        return false;
    }
    bool supported
        = (BitsPerSample == 8 || BitsPerSample == 16 || BitsPerSample == 24 || BitsPerSample == 32);
    if (!supported) {
        return false;
    }
    if ((sfinfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_OGG) {
        return true;
    }
    if ((sfinfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_MPEG) {
        return true;
    }
    if (BitsPerSample == 8) {
        sfinfo.format |= SF_FORMAT_PCM_U8;
    }
    if (BitsPerSample == 16) {
        sfinfo.format |= SF_FORMAT_PCM_16;
    }
    if (BitsPerSample == 24) {
        sfinfo.format |= SF_FORMAT_PCM_24;
    }
    if (BitsPerSample == 32) {
        sfinfo.format |= SF_FORMAT_PCM_32;
    }
    return true;
}
//=============================================================================
static bool
isSupportedDataType(const ArrayOf& data)
{
    NelsonType dataClass = data.getDataClass();
    return (dataClass == NLS_UINT8) || (dataClass == NLS_INT16) || (dataClass == NLS_INT32)
        || (dataClass == NLS_SINGLE) || (dataClass == NLS_DOUBLE);
}
//=============================================================================
bool
AudioWrite(const std::wstring& filename, const ArrayOf& data, int fs, const wstringVector& metadata,
    int BitsPerSample, int BitRate, std::wstring& errorMessage)
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
    indexType rows = data.getRows();
    indexType columns = data.getColumns();
    ArrayOf audioData;
    if (columns > rows) {
        bool needToOverload;
        audioData = ComplexTranspose(data, needToOverload);
    } else {
        audioData = data;
    }
    rows = audioData.getRows();
    columns = audioData.getColumns();
    int nbChannels = static_cast<int>(columns);
    FileSystemWrapper::Path pathFilename = filename;
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
        if (!sf_format_check(&sfinfo)) {
            errorMessage = _W("Invalid parameters values.");
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
            const char* msg = sf_strerror(nullptr);
            errorMessage = utf8_to_wstring(msg);
            return false;
        }
        int BUFFER = 8192;
        int writecount = 0;
        long total = 0;
        size_t idx = 0;
        switch (audioData.getDataClass()) {
        case NLS_UINT8: {
            auto* ptrAudioData = (uint8*)audioData.getDataPointer();
            single* buffer = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, BUFFER * sfinfo.channels, stringVector(), true);
            size_t idx = 0;
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = static_cast<int>(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        single value
                            = (single)((ptrAudioData[idx++] - std::pow(2.0, 7)) / std::pow(2.0, 7));
                        single max_val = (value < single(-1.0)) ? single(-1.0) : value;
                        single min_val = (single(1.0) < max_val) ? single(1.0) : max_val;
                        buffer[(size_t)(k) * ((size_t)sfinfo.channels + (size_t)ch)] = min_val;
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
            auto* ptrAudioData = (int16*)audioData.getDataPointer();
            int16* buffer = (int16*)ArrayOf::allocateArrayOf(
                NLS_INT16, BUFFER * sfinfo.channels, stringVector(), true);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = static_cast<int>(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[(size_t)(k) * ((size_t)sfinfo.channels + (size_t)ch)]
                            = ptrAudioData[idx++];
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
            auto* ptrAudioData = (int32*)audioData.getDataPointer();
            int32* buffer = (int32*)ArrayOf::allocateArrayOf(
                NLS_INT32, BUFFER * sfinfo.channels, stringVector(), true);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = static_cast<int>(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[(size_t)(k) * (size_t)sfinfo.channels + (size_t)(ch)]
                            = ptrAudioData[idx++];
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
            auto* ptrAudioData = (single*)audioData.getDataPointer();
            single* buffer = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, BUFFER * sfinfo.channels, stringVector(), true);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = static_cast<int>(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[(size_t)(k) * (size_t)(sfinfo.channels) + (size_t)(ch)]
                            = ptrAudioData[idx++];
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
            auto* ptrAudioData = (double*)audioData.getDataPointer();
            double* buffer = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, BUFFER * sfinfo.channels, stringVector(), true);
            do {
                writecount = BUFFER;
                if (total + writecount > rows) {
                    writecount = static_cast<int>(rows - total);
                }
                for (int ch = 0; ch < sfinfo.channels; ch++) {
                    for (int k = 0; k < writecount; k++) {
                        buffer[(size_t)(k) * (size_t)sfinfo.channels + (size_t)(ch)]
                            = ptrAudioData[idx++];
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
        default: {
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
    }
    errorMessage = _W("Filename must have an extension.");

    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
