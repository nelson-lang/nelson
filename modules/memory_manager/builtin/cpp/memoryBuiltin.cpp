//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "memoryBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "MemoryInformation.hpp"
#include "NelsonPrint.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::memoryBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0, 0);
    if (nLhs == 0) {
        std::wstring msg;
        unsigned int val;
        val = static_cast<unsigned int>(getTotalPhysicalMemoryByNelson() * 9.54e-7);
        msg = _W("Physical Memory used by Nelson:") + L" " + std::to_wstring(val) + L" MB" + L" ("
            + std::to_wstring(static_cast<uint64>(getTotalPhysicalMemoryByNelson())) + L" bytes)"
            + L"\n";
        NelsonPrint(msg);
        val = static_cast<unsigned int>(getTotalVirtualMemoryByNelson() * 9.54e-7);
        msg = _W("Virtual Memory used by Nelson:") + L" " + std::to_wstring(val) + L" MB" + L" ("
            + std::to_wstring(static_cast<uint64>(getTotalVirtualMemoryByNelson())) + L" bytes)"
            + L"\n";
        NelsonPrint(msg);
        val = static_cast<unsigned int>(getTotalPhysicalMemory() * 9.54e-7);
        msg = _W("Physical Memory:") + L" " + std::to_wstring(val) + L" MB" + L" ("
            + std::to_wstring(static_cast<uint64>(getTotalPhysicalMemory())) + L" bytes)" + L"\n";
        NelsonPrint(msg);
        val = static_cast<unsigned int>(getTotalVirtualMemory() * 9.54e-7);
        msg = _W("Virtual Memory:") + L" " + std::to_wstring(val) + L" MB" + L" ("
            + std::to_wstring(static_cast<uint64>(getTotalVirtualMemory())) + L" bytes)" + L"\n";
        NelsonPrint(msg);
    }
    if (nLhs > 0) {
        wstringVector fieldnames;
        ArrayOfVector fieldvalues;
        uint64 val;
        fieldnames.push_back(L"MaxPossibleArrayBytes");
        val = static_cast<uint64>(getTotalPhysicalMemory() + getTotalVirtualMemory());
        fieldvalues.push_back(ArrayOf::doubleConstructor(static_cast<double>(val)));
        fieldnames.push_back(L"MemAvailableAllArrays");
        val = static_cast<uint64>(getTotalPhysicalMemory() + getTotalVirtualMemory());
        fieldvalues.push_back(ArrayOf::doubleConstructor(static_cast<double>(val)));
        fieldnames.push_back(L"MemUsedNelson");
        fieldvalues.push_back(ArrayOf::doubleConstructor(getTotalPhysicalMemoryByNelson()));
        retval << ArrayOf::structConstructor(fieldnames, fieldvalues);
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
        auto val = static_cast<uint64>(getTotalPhysicalMemory() + getTotalVirtualMemory());
        fieldvalues1.push_back(ArrayOf::doubleConstructor(static_cast<double>(val)));
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
        retval << ArrayOf::structConstructor(fieldnames, fieldvalues);
    }
    return retval;
}
//=============================================================================
