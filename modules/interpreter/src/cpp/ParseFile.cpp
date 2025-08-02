//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif
//=============================================================================
#include "StringHelpers.hpp"
#include "ParseFile.hpp"
#include "Error.hpp"
#include "ParserInterface.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ParserState
ParseFile(Evaluator* eval, const std::wstring& filename, bool bIgnoreException)
{
    ParserState ps = ParserState::ParseError;
    FileSystemWrapper::Path pathFunction(filename);
    bool bIsFile = pathFunction.is_regular_file();
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
        ps = parseFile(eval->lexerContext, fr, wstring_to_utf8(filename));
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
            if (StringHelpers::iequals(functionNameFromFile, cp->getName())) {
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
