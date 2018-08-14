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
#include "DebugStack.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void DebugStack(const std::vector<StackEntry> &cstack, int nbOmitLines, stackTrace& stackPositions)
{
  stackPositions.clear();
  size_t i = 0;
  while (i < cstack.size()) {
    if (cstack[i].tokid == 0) {
      size_t j = i + 1;
      while ((j < cstack.size()) && (cstack[j].cname == cstack[i].cname) &&
             (cstack[j].detail == cstack[i].detail) && (cstack[j].tokid != 0)) {
        j++;
      }
      std::wstring filename = utf8_to_wstring(cstack[j - 1].cname.c_str());
      std::wstring functionname = utf8_to_wstring(cstack[j - 1].detail.c_str());
      int lineposition = cstack[j - 1].tokid & 0x0000FFFF;

      if (!(filename == std::wstring(L"EvaluateString") && lineposition == 1)) {
        stackPositions.push_back(
            PositionScript(functionname, filename, lineposition));
      }
      i = j;
    } else {
      i++;
    }
  }
  for (int k = 0; k < nbOmitLines; k++) {
    stackPositions.pop_back();
  }
  std::reverse(std::begin(stackPositions), std::end(stackPositions));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
