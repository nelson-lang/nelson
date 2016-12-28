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
#include <boost/format.hpp>
#include "calendarBuiltin.hpp"
#include "Calendar.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::TimeGateway::calendarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    Calendar *cal = nullptr;
    if (nLhs > 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size())
    {
        case 0:
        {
            cal = new Calendar();
        }
        break;
        case 1:
        {
            ArrayOf param1 = argIn[0];
            double d = param1.getContentsAsDoubleScalar();
            cal = new Calendar(d);
        }
        break;
        case 2:
        {
            ArrayOf param1 = argIn[0];
            uint64 cyear = param1.getContentsAsUnsignedInt64Scalar();
            if (cyear < 1400 || cyear > 9999)
            {
                Error(eval, ERROR_WRONG_ARGUMENT_1_VALUE);
            }
            ArrayOf param2 = argIn[1];
            int32 cmonth = param2.getContentsAsInteger32Scalar();
            if (cmonth > 0 && cmonth < 13)
            {
                cal = new Calendar(cyear, (uint8)cmonth);
            }
            else
            {
                Error(eval, ERROR_WRONG_ARGUMENT_2_VALUE);
            }
        }
        break;
        default:
        {
            Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        break;
    }
    if (cal == nullptr)
    {
        Error(eval, _W("Calendar not initialized."));
    }
    if (cal->getYear() < 1400 || cal->getYear() > 9999)
    {
        Error(eval, _W("Year value is wrong [1400, 9999] expected."));
    }
    if (!(cal->getMonth() > 0 && cal->getMonth() < 13))
    {
        Error(eval, _W("Month value is wrong [1, 12] expected."));
    }
    switch (nLhs)
    {
        case 0:
        {
            std::wstring msg = cal->getAsFormatedText();
            delete cal;
            Interface *io = eval->getInterface();
            if (io)
            {
                io->outputMessage(msg);
            }
        }
        break;
        case 1:
        {
            retval.push_back(cal->get());
            delete cal;
        }
        break;
        case 2:
        {
            retval.push_back(cal->get());
            retval.push_back(ToCellStringAsRow(cal->getNameOfDays()));
            delete cal;
        }
        break;
        default:
        {
            Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        break;
    }
    return retval;
}
//=============================================================================
