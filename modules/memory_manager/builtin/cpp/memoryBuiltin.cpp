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
#include "memoryBuiltin.hpp"
#include "Error.hpp"
#include "MemoryInformation.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::memoryBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring msg;
            unsigned int val;
            val = (unsigned int)(getTotalPhysicalMemoryByNelson() * 9.54e-7);
            msg = _W("Physical Memory used by Nelson:") + L" " + std::to_wstring(val) + L" MB"
                + L" (" + std::to_wstring((uint64)getTotalPhysicalMemoryByNelson()) + L" bytes)"
                + L"\n";
            io->outputMessage(msg);
            val = (unsigned int)(getTotalVirtualMemoryByNelson() * 9.54e-7);
            msg = _W("Virtual Memory used by Nelson:") + L" " + std::to_wstring(val) + L" MB"
                + L" (" + std::to_wstring((uint64)getTotalVirtualMemoryByNelson()) + L" bytes)"
                + L"\n";
            io->outputMessage(msg);
            val = (unsigned int)(getTotalPhysicalMemory() * 9.54e-7);
            msg = _W("Physical Memory:") + L" " + std::to_wstring(val) + L" MB" + L" ("
                + std::to_wstring((uint64)getTotalPhysicalMemory()) + L" bytes)" + L"\n";
            io->outputMessage(msg);
            val = (unsigned int)(getTotalVirtualMemory() * 9.54e-7);
            msg = _W("Virtual Memory:") + L" " + std::to_wstring(val) + L" MB" + L" ("
                + std::to_wstring((uint64)getTotalVirtualMemory()) + L" bytes)" + L"\n";
            io->outputMessage(msg);
        }
    }
    if (nLhs > 0) {
        wstringVector fieldnames;
        ArrayOfVector fieldvalues;
        uint64 val;
        fieldnames.push_back(L"MaxPossibleArrayBytes");
        val = (uint64)(getTotalPhysicalMemory() + getTotalVirtualMemory());
        fieldvalues.push_back(ArrayOf::doubleConstructor((double)val));
        fieldnames.push_back(L"MemAvailableAllArrays");
        val = (uint64)(getTotalPhysicalMemory() + getTotalVirtualMemory());
        fieldvalues.push_back(ArrayOf::doubleConstructor((double)val));
        fieldnames.push_back(L"MemUsedNelson");
        fieldvalues.push_back(ArrayOf::doubleConstructor(getTotalPhysicalMemoryByNelson()));
        retval.push_back(ArrayOf::structConstructor(fieldnames, fieldvalues));
    }
    if (nLhs > 1) {
        wstringVector fieldnames;
        ArrayOfVector fieldvalues;
        wstringVector fieldnames1;
        ArrayOfVector fieldvalues1;
        fieldnames.push_back(L"VirtualAddressSpace");
        fieldnames1.push_back(L"Available");
        fieldnames1.push_back(L"Total");
        fieldvalues1.push_back(
            ArrayOf::doubleConstructor(getTotalVirtualMemory() - getTotalVirtualMemoryUsed()));
        fieldvalues1.push_back(ArrayOf::doubleConstructor(getTotalVirtualMemory()));
        fieldvalues.push_back(ArrayOf::structConstructor(fieldnames1, fieldvalues1));
        fieldnames1.clear();
        fieldvalues1.clear();
        fieldnames.push_back(L"SystemMemory");
        fieldnames1.push_back(L"Available");
        uint64 val = (uint64)(getTotalPhysicalMemory() + getTotalVirtualMemory());
        fieldvalues1.push_back(ArrayOf::doubleConstructor((double)val));
        fieldvalues.push_back(ArrayOf::structConstructor(fieldnames1, fieldvalues1));
        fieldnames1.clear();
        fieldvalues1.clear();
        fieldnames.push_back(L"PhysicalMemory");
        fieldnames1.push_back(L"Available");
        fieldnames1.push_back(L"Total");
        fieldvalues1.push_back(
            ArrayOf::doubleConstructor(getTotalPhysicalMemory() - getTotalPhysicalMemoryUsed()));
        fieldvalues1.push_back(ArrayOf::doubleConstructor(getTotalPhysicalMemory()));
        fieldvalues.push_back(ArrayOf::structConstructor(fieldnames1, fieldvalues1));
        retval.push_back(ArrayOf::structConstructor(fieldnames, fieldvalues));
    }
    return retval;
}
//=============================================================================
