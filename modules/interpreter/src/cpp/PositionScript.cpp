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
#include "PositionScript.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PositionScript::PositionScript(std::wstring functionname, std::wstring filename,
                               int line) {
  this->filename = filename;
  this->functionname = functionname;
  this->line = line;
}
//=============================================================================
PositionScript::~PositionScript() {
  this->filename = L"";
  this->functionname = L"";
  this->line = -1;
}
//=============================================================================
PositionScript::PositionScript(const PositionScript &copy) {
  this->filename = copy.filename;
  this->functionname = copy.functionname;
  this->line = copy.line;
}
//=============================================================================
void PositionScript::operator=(const PositionScript &copy) {
  if (this == &copy) {
    return;
  }
  this->filename = copy.filename;
  this->functionname = copy.functionname;
  this->line = copy.line;
}
//=============================================================================
std::wstring PositionScript::getFilename() { return this->filename; }
//=============================================================================
void PositionScript::setFilename(std::wstring filename) {
  this->filename = filename;
}
//=============================================================================
int PositionScript::getLine() { return this->line; }
//=============================================================================
void PositionScript::setLine(int _line) { this->line = _line; }
//=============================================================================
void PositionScript::setFunctionName(std::wstring functionname) {
  this->functionname = functionname;
}
//=============================================================================
std::wstring PositionScript::getFunctionName() { return this->functionname; }
//=============================================================================
bool PositionScript::isEmpty() {
  return (this->functionname == L"" && this->filename == L"" &&
          this->line == -1);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
