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
#include "AudioFileInfo.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <fileref.h>
#include <sndfile.h>
#include <tag.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
getCompressionMethodAsString(int format)
{
    std::wstring compressionMethod(L"");
    switch (format & SF_FORMAT_TYPEMASK) {
    case SF_FORMAT_WAV:
        compressionMethod = L"wav";
        break; /* Microsoft WAV format (little endian default). */
    case SF_FORMAT_AIFF:
        compressionMethod = L"aiff";
        break; /* Apple/SGI AIFF format (big endian). */
    case SF_FORMAT_AU:
        compressionMethod = L"au";
        break; /* Sun/NeXT AU format (big endian). */
    case SF_FORMAT_RAW:
        compressionMethod = L"raw";
        break; /* RAW PCM data. */
    case SF_FORMAT_PAF:
        compressionMethod = L"paf";
        break; /* Ensoniq PARIS file format. */
    case SF_FORMAT_SVX:
        compressionMethod = L"svx";
        break; /* Amiga IFF / SVX8 / SV16 format. */
    case SF_FORMAT_NIST:
        compressionMethod = L"nist";
        break; /* Sphere NIST format. */
    case SF_FORMAT_VOC:
        compressionMethod = L"voc";
        break; /* VOC files. */
    case SF_FORMAT_IRCAM:
        compressionMethod = L"ircam";
        break; /* Berkeley/IRCAM/CARL */
    case SF_FORMAT_W64:
        compressionMethod = L"w64";
        break; /* Sonic Foundry's 64 bit RIFF/WAV */
    case SF_FORMAT_MAT4:
        compressionMethod = L"mat4";
        break; /* Matlab (tm) V4.2 / GNU Octave 2.0 */
    case SF_FORMAT_MAT5:
        compressionMethod = L"mat5";
        break; /* Matlab (tm) V5.0 / GNU Octave 2.1 */
    case SF_FORMAT_PVF:
        compressionMethod = L"pvf";
        break; /* Portable Voice Format */
    case SF_FORMAT_XI:
        compressionMethod = L"xi";
        break; /* Fasttracker 2 Extended Instrument */
    case SF_FORMAT_HTK:
        compressionMethod = L"htk";
        break; /* HMM Tool Kit format */
    case SF_FORMAT_SDS:
        compressionMethod = L"sds";
        break; /* Midi Sample Dump Standard */
    case SF_FORMAT_AVR:
        compressionMethod = L"avr";
        break; /* Audio Visual Research */
    case SF_FORMAT_WAVEX:
        compressionMethod = L"wavx";
        break; /* MS WAVE with WAVEFORMATEX */
    case SF_FORMAT_SD2:
        compressionMethod = L"sd2";
        break; /* Sound Designer 2 */
    case SF_FORMAT_FLAC:
        compressionMethod = L"flac";
        break; /* FLAC lossless file format */
    case SF_FORMAT_CAF:
        compressionMethod = L"caf";
        break; /* Core Audio File format */
    case SF_FORMAT_WVE:
        compressionMethod = L"wfe";
        break; /* Psion WVE format */
    case SF_FORMAT_OGG:
        compressionMethod = L"ogg";
        break; /* Xiph OGG container */
    case SF_FORMAT_MPC2K:
        compressionMethod = L"mpc2k";
        break; /* Akai MPC 2000 sampler */
    case SF_FORMAT_RF64:
        compressionMethod = L"rf64";
        break; /* RF64 WAV file */
    default:
        compressionMethod = L"unknown";
        break;
    };
    boost::to_upper(compressionMethod);
    return compressionMethod;
}
//=============================================================================
static double
getBitsPerSample(int format)
{
    double bits = -1;
    switch (format & SF_FORMAT_SUBMASK) {
    case SF_FORMAT_PCM_S8:
        bits = 8;
        break; /* Signed 8 bit data */
    case SF_FORMAT_PCM_16:
        bits = 16;
        break; /* Signed 16 bit data */
    case SF_FORMAT_PCM_24:
        bits = 24;
        break; /* Signed 24 bit data */
    case SF_FORMAT_PCM_32:
        bits = 32;
        break; /* Signed 32 bit data */
    case SF_FORMAT_PCM_U8:
        bits = 8;
        break; /* Unsigned 8 bit data (WAV and RAW only) */
    case SF_FORMAT_FLOAT:
        bits = 32;
        break; /* 32 bit float data */
    case SF_FORMAT_DOUBLE:
        bits = 64;
        break; /* 64 bit float data */
    case SF_FORMAT_ULAW:
        bits = 8;
        break; /* U-Law encoded. */
    case SF_FORMAT_ALAW:
        bits = 8;
        break; /* A-Law encoded. */
    case SF_FORMAT_IMA_ADPCM:
        bits = 4;
        break; /* IMA ADPCM. */
    case SF_FORMAT_MS_ADPCM:
        bits = 8;
        break; /* Microsoft ADPCM. */
    case SF_FORMAT_GSM610:
        bits = -1;
        break; /* GSM 6.10 encoding. */
    case SF_FORMAT_VOX_ADPCM:
        bits = -1;
        break; /* OKI / Dialogix ADPCM */
    case SF_FORMAT_G721_32:
        bits = 32;
        break; /* 32kbs G721 ADPCM encoding. */
    case SF_FORMAT_G723_24:
        bits = 24;
        break; /* 24kbs G723 ADPCM encoding. */
    case SF_FORMAT_G723_40:
        bits = 40;
        break; /* 40kbs G723 ADPCM encoding. */
    case SF_FORMAT_DWVW_12:
        bits = 12;
        break; /* 12 bit Delta Width Variable Word encoding. */
    case SF_FORMAT_DWVW_16:
        bits = 16;
        break; /* 16 bit Delta Width Variable Word encoding. */
    case SF_FORMAT_DWVW_24:
        bits = 24;
        break; /* 24 bit Delta Width Variable Word encoding. */
    case SF_FORMAT_DWVW_N:
        bits = -1;
        break; /* N bit Delta Width Variable Word encoding. */
    case SF_FORMAT_DPCM_8:
        bits = 8;
        break; /* 8 bit differential PCM (XI only) */
    case SF_FORMAT_DPCM_16:
        bits = 16;
        break; /* 16 bit differential PCM (XI only) */
    case SF_FORMAT_VORBIS:
        bits = 16;
        break; /* Xiph Vorbis encoding. */
    default:
        bits = -1;
        break;
    }
    return bits;
}
//=============================================================================
ArrayOf
AudioFileInfo(std::wstring filename, std::wstring& errorMessage)
{
    Dimensions dims(0, 0);
    ArrayOf res = ArrayOf::emptyConstructor(dims);
    errorMessage = L"";
    SNDFILE* file = nullptr;
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
        return res;
    }
    std::wstring CompressionMethod = getCompressionMethodAsString(sfinfo.format);
    double NumChannels = sfinfo.channels;
    double SampleRate = sfinfo.samplerate;
    double TotalSamples = (double)sfinfo.frames;
    double Duration = TotalSamples / SampleRate;
    double BitsPerSample = getBitsPerSample(sfinfo.format);
    sf_close(file);
    std::wstring Title;
    std::wstring Comment;
    std::wstring Artist;
#ifdef _MSC_VER
    TagLib::FileRef f(filename.c_str());
#else
    TagLib::FileRef f(wstring_to_utf8(filename).c_str());
#endif
    if (!f.isNull() && f.tag()) {
        TagLib::Tag* tag = f.tag();
        if (tag) {
            Title = tag->title().toWString();
            Comment = tag->comment().toWString();
            Artist = tag->artist().toWString();
        }
    }
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("Filename");
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(filename));
    fieldnames.push_back("CompressionMethod");
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(CompressionMethod));
    fieldnames.push_back("NumChannels");
    fieldvalues.push_back(ArrayOf::doubleConstructor(NumChannels));
    fieldnames.push_back("SampleRate");
    fieldvalues.push_back(ArrayOf::doubleConstructor(SampleRate));
    fieldnames.push_back("TotalSamples");
    fieldvalues.push_back(ArrayOf::doubleConstructor(TotalSamples));
    fieldnames.push_back("Duration");
    fieldvalues.push_back(ArrayOf::doubleConstructor(Duration));
    fieldnames.push_back("Title");
    if (Title == L"") {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    } else {
        fieldvalues.push_back(ArrayOf::characterArrayConstructor(Title));
    }
    fieldnames.push_back("Comment");
    if (Comment == L"") {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    } else {
        fieldvalues.push_back(ArrayOf::characterArrayConstructor(Comment));
    }
    fieldnames.push_back("Artist");
    if (Artist == L"") {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    } else {
        fieldvalues.push_back(ArrayOf::characterArrayConstructor(Artist));
    }
    fieldnames.push_back("BitsPerSample");
    fieldvalues.push_back(ArrayOf::doubleConstructor(BitsPerSample));
    res = ArrayOf::structConstructor(fieldnames, fieldvalues);
    return res;
}
//=============================================================================
}
//=============================================================================
