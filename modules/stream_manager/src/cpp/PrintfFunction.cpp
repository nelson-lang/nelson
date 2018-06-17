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
#include "PrintfFunction.hpp"
#include "Exception.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool flagChar(wchar_t c) {
  return ((c == L'#') || (c == L'0') || (c == L'-') || (c == L' ') ||
          (c == L'+'));
}
//=============================================================================
static bool convspec(wchar_t c) {
  return ((c == L'd') || (c == L'i') || (c == L'o') || (c == L'u') ||
          (c == L'x') || (c == L'X') || (c == L'e') || (c == L'E') ||
          (c == L'f') || (c == L'F') || (c == L'g') || (c == L'G') ||
          (c == L'a') || (c == L'A') || (c == L'c') || (c == L's'));
}
//=============================================================================
static wchar_t *validateFormatSpec(wchar_t *cp) {
    if (*cp == L'%') {
        return cp + 1;
    }
    while ((*cp) && flagChar(*cp)) {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    while ((*cp) && (*cp == L'.')) {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    if ((*cp) && convspec(*cp)) {
        return cp + 1;
    }
    return 0;
}
//=============================================================================
bool printfFunction(const ArrayOfVector &arg, std::wstring &errorMessage,
                    std::wstring &result) {
  errorMessage = L"";
  ArrayOf format(arg[0]);
  std::wstring frmt = format.getContentAsWideString();
  wchar_t *buff = new wchar_t[wcslen(frmt.c_str()) + 1];
  wcscpy(buff, frmt.c_str());
  wchar_t *dp = buff;
  wchar_t nbuff[8192];
  int nextArg = 1;
  while (*dp) {
    wchar_t *np = dp;
    while ((*dp) && (*dp != L'%')) {
      dp++;
    }
    wchar_t sv = *dp;
    *dp = 0;
    result = result + std::wstring(np);
    *dp = sv;
    if (*dp) {
      np = validateFormatSpec(dp + 1);
      if (!np) {
        errorMessage = _W("Erroneous format specification ") + std::wstring(dp);
        return false;
      } else {
        if (*(np - 1) == L'%') {
          result = result + std::wstring(L"%%");
          dp += 2;
        } else {
          sv = *np;
          *np = 0;
          if (arg.size() <= nextArg) {
            errorMessage =
                _W("Not enough arguments to satisfy format specification");
            return false;
          }
          ArrayOf nextValue(arg[nextArg++]);
          if ((*(np - 1) != 's') && (nextValue.isEmpty())) {
            result = result + std::wstring(L"[]");
          } else {
            switch (*(np - 1)) {
            case 'u': {
              std::wstring f = dp;
              boost::replace_all(f, L"u", L"llu");
              nextValue.promoteType(NLS_INT64);
              long long val = ((long long *)nextValue.getDataPointer())[0];
              swprintf(nbuff, f.c_str(), val);
              result = result + std::wstring(nbuff);
            } break;
            case 'd': {
              std::wstring f = dp;
              boost::replace_all(f, L"d", L"lld");
              nextValue.promoteType(NLS_INT64);
              long long val = ((long long*)nextValue.getDataPointer())[0];
              swprintf(nbuff, f.c_str(), val);
              result = result + std::wstring(nbuff);
            } break;
            case 'i':
            case 'o':
            case 'x':
            case 'X':
            case 'c': {
              nextValue.promoteType(NLS_INT64);
              swprintf(nbuff, dp, *((int64 *)nextValue.getDataPointer()));
              result = result + std::wstring(nbuff);
            } break;
            case 'e':
            case 'E':
            case 'f':
            case 'F':
            case 'g':
            case 'G': {
              nextValue.promoteType(NLS_DOUBLE);
              swprintf(nbuff, dp, *((double *)nextValue.getDataPointer()));
              result = result + std::wstring(nbuff);
            } break;
            case 's': {
              std::wstring f = dp;
              boost::replace_all(f, L"s", L"ls");
              std::wstring wstr = nextValue.getContentAsWideString();
              swprintf(nbuff, f.c_str(), wstr.c_str());
              result = result + std::wstring(nbuff);
            } break;
            }
          }
          *np = sv;
          dp = np;
        }
      }
    }
  }
  delete[] buff;
  return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
