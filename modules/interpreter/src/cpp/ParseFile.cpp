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
#include "ParseFile.hpp"
#include "Error.hpp"
#include "ParserInterface.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
ParserState
ParseFile(Evaluator* eval, std::wstring filename, bool bIgnoreException)
{
    ParserState ps = ParserState::ParseError;
    boost::filesystem::path pathFunction(filename);
    bool bIsFile
        = boost::filesystem::exists(pathFunction) && !boost::filesystem::is_directory(pathFunction);
    if (!bIsFile) {
        return ParserState::ParseError;
    }
    FILE* fr;
#ifdef _MSC_VER
    fr = _wfopen(filename.c_str(), L"rt");
#else
    fr = fopen(wstring_to_utf8(filename).c_str(), "rt");
#endif
    if (!fr) {
        return ParserState::ParseError;
    }
    Exception previousException(eval->getLastErrorException());
    try {
        ps = parseFile(fr, wstring_to_utf8(filename).c_str());
    } catch (const Exception&) {
        if (bIgnoreException) {
            eval->setLastErrorException(previousException);
        }
        ps = ParserState::ParseError;
    }
    fclose(fr);
    if (ps == ParserState::FuncDef) {
        MacroFunctionDef* cp = getParsedFunctionDef();
        if (cp) {
            std::string functionNameFromFile = pathFunction.stem().generic_string();
            if (boost::iequals(functionNameFromFile, cp->name)) {
                ps = ParserState::FuncDef;
            } else {
                ps = ParserState::ParseError;
            }
            delete cp;
            cp = nullptr;
        } else {
            ps = ParserState::ScriptBlock;
        }
    }
    return ps;
}
//=============================================================================
}
//=============================================================================
