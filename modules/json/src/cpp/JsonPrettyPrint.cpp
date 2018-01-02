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
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include "JsonPrettyPrint.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	ArrayOf jsonPrettyPrint(std::wstring stringToDecode, std::wstring &errorMessage)
	{
		errorMessage = L"";
		std::wistringstream is(stringToDecode);
		boost::property_tree::wptree pt;
		try
		{
			boost::property_tree::read_json(is, pt);
		}
		catch (boost::property_tree::json_parser::json_parser_error &je)
		{
			errorMessage = utf8_to_wstring(je.message());
			return ArrayOf::stringConstructor("");
		}
		std::wostringstream buf;
		boost::property_tree::write_json(buf, pt, true);
		std::wstring jsonPrettified = buf.str();
		return ArrayOf::stringConstructor(jsonPrettified);
	}
	//=============================================================================
}
//=============================================================================
