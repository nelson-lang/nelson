//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
ParseFile(Evaluator* eval, const std::wstring& filename, bool bIgnoreException)
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
    if (fr == nullptr) {
        return ParserState::ParseError;
    }
    Exception previousException(eval->getLastErrorException());
    try {
        ps = parseFile(fr, wstring_to_utf8(filename));
    } catch (const Exception&) {
        if (bIgnoreException) {
            eval->setLastErrorException(previousException);
        }
        ps = ParserState::ParseError;
    }
    fclose(fr);
    if (ps == ParserState::FuncDef) {
        MacroFunctionDef* cp = getParsedFunctionDef();
        if (cp != nullptr) {
            std::string functionNameFromFile = pathFunction.stem().generic_string();
            if (boost::iequals(functionNameFromFile, cp->getName())) {
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
} // namespace Nelson
//=============================================================================
