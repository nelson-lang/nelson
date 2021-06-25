%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('fgetl'), 1);
assert_isequal(nargout('fgetl'), 1);
%=============================================================================
filename = [modulepath('characters_encoding'), '/tests/shisei_UTF-8.txt'];
assert_isequal(isfile(filename), true);
fid = fopen(filename, 'rt', 'n', 'UTF-8');
found = false;
while ~feof(fid)
  line = fgetl(fid);
  if (~isdouble(line))
    if (startsWith(line, 'Eg. ［＃ここから３字下げ］'))
      found = true;
      break;
    end
  end
end
fclose(fid);
assert_istrue(found);
%=============================================================================
filename = [modulepath('characters_encoding'), '/tests/olaf_Windows-1251.txt'];
assert_isequal(isfile(filename), true);
fid = fopen(filename, 'rt', 'n', 'windows-1251');
found = false;
while ~feof(fid)
  line = fgetl(fid);
  if (~isdouble(line))
    if (startsWith(line, 'P.S. Когато преглеждах първата'))
      found = true;
      break;
    end
  end
end
fclose(fid);
assert_istrue(found);
%=============================================================================
