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
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static int getErrorLinePosition(Evaluator *eval)
    {
        int Line = -1;
        if (eval->cstack.size() >= 2)
        {
            try
            {
                Line = eval->cstack[eval->cstack.size() - 2].tokid & 0x0000FFFF;
            }
            catch (std::runtime_error &e)
            {
                e.what();
                Line = -1;
            }
        }
        return Line;
    }
    //=============================================================================
    static int getErrorColumnPosition(Evaluator *eval)
    {
        int Pos = -1;
        int isize = (int)eval->cstack.size();
        if (isize - 2 >= 0)
        {
            Pos = eval->cstack[isize - 2].tokid >> 16;
        }
        return Pos;
    }
    //=============================================================================
    static std::wstring getCurrentEvaluateFilename(Evaluator *eval)
    {
        int isize = (int)eval->evaluatedFilenames.size();
        if (isize - 1 >= 0)
        {
            return eval->evaluatedFilenames[isize - 1];
        }
        return L"";
    }
    //=============================================================================
    static std::wstring getErrorFunctionName(Evaluator *eval)
    {
        std::wstring wfunctionname;
        int isize = (int)eval->cstack.size();
        if (isize - 2 >= 0)
        {
            wfunctionname = utf8_to_wstring(eval->cstack[isize - 2].cname);
        }
        //wfunctionname = utf8_to_wstring(eval->getContext()->getCurrentScope()->getName());
        return wfunctionname;
    }
    //=============================================================================
    static std::wstring getErrorFilename(Evaluator *eval)
    {
        std::wstring wfilename;
        if (getErrorFunctionName(eval) == L"EvaluateScript")
        {
            wfilename = getCurrentEvaluateFilename(eval);
        }
        else
        {
            wfilename = getErrorFunctionName(eval);
        }
        return wfilename;
    }
    //=============================================================================
    void Error(Evaluator *eval, std::wstring msg)
    {
        int LinePosition = -1;
        int ColumnPosition = -1;
        std::wstring FileName = L"";
        std::wstring FunctionName = getErrorFunctionName(eval);
        if (FunctionName != L"EvaluateScript")
        {
            LinePosition = getErrorLinePosition(eval);
            ColumnPosition = getErrorColumnPosition(eval);
            FileName = getErrorFilename(eval);
        }
        throw Exception(msg, FunctionName, LinePosition, ColumnPosition, FileName);
    }
    //=============================================================================
    void Error(Evaluator *eval, std::wstring msg, std::wstring functionname)
    {
        int LinePosition = -1;
        int ColumnPosition = -1;
        std::wstring FileName = L"";
        std::wstring FunctionName = getErrorFunctionName(eval);
        if (FunctionName == functionname)
        {
            LinePosition = getErrorLinePosition(eval);
            ColumnPosition = getErrorColumnPosition(eval);
            FileName = getErrorFilename(eval);
        }
        else
        {
            FunctionName = functionname;
        }
        throw Exception(msg, FunctionName, LinePosition, ColumnPosition, FileName);
    }
    //=============================================================================
    void Error(Evaluator *eval, std::string msg)
    {
        Error(eval, utf8_to_wstring(msg));
    }
    //=============================================================================
    void Error(Evaluator *eval, std::string msg, std::string functionname)
    {
        Error(eval, utf8_to_wstring(msg), utf8_to_wstring(functionname));
    }
    //=============================================================================
    void updateError(Evaluator *eval, Exception &e)
    {
        std::wstring FunctionName = getErrorFunctionName(eval);
        if (FunctionName != L"EvaluateScript")
        {
            int LinePosition = getErrorLinePosition(eval);
            int ColumnPosition = getErrorColumnPosition(eval);
            std::wstring FileName = getErrorFilename(eval);
            e.setFunctionName(FunctionName);
            e.setFileName(FileName);
            e.setLinePosition(LinePosition, ColumnPosition);
        }
    }
    //=============================================================================
}
//=============================================================================
