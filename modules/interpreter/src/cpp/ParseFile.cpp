//=============================================================================
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include "ParseFile.hpp"
#include "Exception.hpp"
#include "ParserInterface.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ParserState ParseFile(Evaluator* eval, std::wstring filename, bool bIgnoreException)
    {
        ParserState ps = ParserState::ParseError;
        boost::filesystem::path pathFunction(filename);
        bool bIsFile = boost::filesystem::exists(pathFunction) && !boost::filesystem::is_directory(pathFunction);
        if (!bIsFile)
        {
            return ParserState::ParseError;
        }
        FILE *fr = nullptr;
#ifdef _MSC_BUILD
        fr = _wfopen(filename.c_str(), L"rt");
#else
        fr = fopen(wstring_to_utf8(filename).c_str(), "rt");
#endif
        if (!fr)
        {
            return ParserState::ParseError;
        }
        Exception previousException(eval->getLastException());
        try
        {
            ps = parseFile(fr, wstring_to_utf8(filename).c_str());
        }
        catch (const Exception &e)
        {
            if (bIgnoreException)
            {
                eval->setLastException(previousException);
            }
            ps = ParserState::ParseError;
        }
        fclose(fr);
        if (ps == ParserState::FuncDef)
        {
            MacroFunctionDef *cp = getParsedFunctionDef();
            if (cp)
            {
                std::string functionNameFromFile = pathFunction.stem().generic_string();
                if (boost::iequals(functionNameFromFile, cp->name))
                {
                    ps = ParserState::FuncDef;
                }
                else
                {
                    ps = ParserState::ParseError;
                }
                delete cp;
                cp = nullptr;
            }
            else
            {
                ps = ParserState::ScriptBlock;
            }
        }
        return ps;
    }
    //=============================================================================
}
//=============================================================================
