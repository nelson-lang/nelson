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
#include "fprintfBuiltin.hpp"
#include "Error.hpp"
#include "FilesManager.hpp"
#include "File.hpp"
#include "Interface.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::StreamGateway::fprintfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn.size() < 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    double dID = 1;
    if (param1.isDoubleType() && param1.isScalar())
    {
        dID = param1.getContentsAsDoubleScalar();
    }
    ArrayOf param2 = argIn[1];
    std::string msg;
    if (!param2.isString())
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    msg = param2.getContentsAsCString();
    FilesManager *fm = (FilesManager *)(eval->FileManager);
    int32 iValue = (int32)dID;
    if (fm == nullptr)
    {
        Error(eval, _W("Problem with file manager."));
    }
    if (fm->isOpened(iValue))
    {
        File *f = fm->getFile(iValue);
        if (f->isInterfaceMethod())
        {
            if ((f->getFileName() == L"stdout") || (f->getFileName() == L"stderr"))
            {
                Interface *io = eval->getInterface();
                if (io)
                {
                    if (f->getFileName() == L"stdout")
                    {
                        io->outputMessage(msg);
                    }
                    else
                    {
                        io->errorMessage(msg);
                    }
                }
            }
            else
            {
                Error(eval, _W("ID not supported."));
            }
        }
        else
        {
            FILE *filepointer = (FILE*)f->getFilePointer();
            if (filepointer)
            {
				fprintf(filepointer, "%s", msg.c_str());
            }
            else
            {
                Error(eval, _W("ID not supported."));
            }
        }
    }
    else
    {
        Error(eval, _W("Wrong value for #1 argument: a valid file ID expected."));
    }
    return retval;
}
//=============================================================================
