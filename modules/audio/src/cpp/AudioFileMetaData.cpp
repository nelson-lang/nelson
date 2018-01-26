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
#include <sndfile.h>
#include "AudioFileMetaData.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
	wstringVector AudioFileMetaData(std::wstring filename, std::wstring &errorMessage)
	{
		wstringVector outputMetaData;
		errorMessage = L"";
		SNDFILE * file = nullptr;
		SF_INFO sfinfo;
		memset(&sfinfo, 0, sizeof(sfinfo));
#ifdef _MSC_VER
		file = sf_wchar_open(filename.c_str(), SFM_READ, &sfinfo);
#else
		std::string ufilename = wstring_to_utf8(filename);
		file = sf_open(ufilename.c_str(), SFM_READ, &sfinfo);
#endif
		if (file == nullptr)
		{
			const char* msg = sf_strerror(NULL);
			errorMessage = utf8_to_wstring(msg);
			return outputMetaData;
		}

		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_TITLE)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_COMMENT)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_ARTIST)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_COPYRIGHT)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_SOFTWARE)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_DATE)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_ALBUM)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_LICENSE)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_TRACKNUMBER)));
		outputMetaData.push_back(utf8_to_wstring(sf_get_string(file, SF_STR_GENRE)));
		sf_close(file);
		return outputMetaData;
	}
	//=============================================================================
}
//=============================================================================
