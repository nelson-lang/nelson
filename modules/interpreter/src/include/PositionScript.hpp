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
#pragma once
//=============================================================================
#include "nlsInterpreter_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP PositionScript {
private:
  std::wstring filename = L"";
  std::wstring functionname = L"";
  int line = -1;

public:
  PositionScript(std::wstring functionname = L"", std::wstring filename = L"",
                 int line = -1);
  PositionScript(const PositionScript &copy);

  void operator=(const PositionScript& copy);
  ~PositionScript();

  std::wstring getFilename();
  void setFilename(std::wstring filename);

  int getLine();
  void setLine(int _line);

  void setFunctionName(std::wstring functionname);
  std::wstring getFunctionName();

  bool isEmpty();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
