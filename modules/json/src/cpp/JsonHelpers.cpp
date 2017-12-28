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
#define JSMN_PARENT_LINKS
#include <jsmn.h>
#include <boost/algorithm/string.hpp>
#include "JsonHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    unsigned int json_parse_item(jsmntok_t *t, ArrayOf **obj)
    {
        return 0;
    }
    //=============================================================================
    ArrayOf jsonDecode(std::wstring stringToDecode, std::wstring &errorMessage)
    {
        std::string strToDecode = wstring_to_utf8(stringToDecode);
        jsmn_parser parserJson;
        jsmn_init(&parserJson);
        int nbTokensOrError = jsmn_parse(&parserJson, strToDecode.c_str(), strToDecode.size(), NULL, 0);
        if (nbTokensOrError > 0)
        {
            jsmntok_t *tokens = new jsmntok_t[nbTokensOrError];
            int nbTokensUsed = jsmn_parse(&parserJson, strToDecode.c_str(), strToDecode.size(), tokens, nbTokensOrError);
            if (nbTokensUsed < 1 || tokens[0].type != JSMN_OBJECT)
            {
                errorMessage = L"Object expected.";
                return ArrayOf();
            }
            else
            {
            }
            delete[] tokens;
        }
        else
        {
            switch (nbTokensOrError)
            {
                case JSMN_ERROR_INVAL:
                {
                    errorMessage = L"Invalid character inside JSON string.";
                }
                break;
                case JSMN_ERROR_NOMEM:
                {
                    errorMessage = L"Not enough tokens were provided.";
                }
                break;
                case JSMN_ERROR_PART:
                {
                    errorMessage = L"The string is not a full JSON packet, more bytes expected.";
                }
                break;
                default:
                {
                    errorMessage = L"Unknow error.";
                }
                break;
            }
        }
        return ArrayOf();
    }
    //=============================================================================
}
//=============================================================================
