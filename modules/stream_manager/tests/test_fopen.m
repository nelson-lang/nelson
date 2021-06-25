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
assert_isequal(nargin('fopen'), 4);
assert_isequal(nargout('fopen'), 4);
%=============================================================================
if (isfile([tempdir(), 'test_fopen.txt']))
  rmfile([tempdir(), 'test_fopen.txt']);
end
%=============================================================================
ID = fopen([tempdir(), 'test_fopen.txt']);
assert_isequal(ID, -1);
%=============================================================================
[ID, MSG] = fopen([tempdir(), 'test_fopen.txt']);
assert_isequal(ID, -1);
assert_isequal(MSG, _('Impossible to open file.'));
%=============================================================================
ID = fopen([tempdir(), 'test_fopen.txt'], 'w');
assert_isequal(ID, 3);
%=============================================================================
filename = fopen(ID);
if ismac()
  assert_istrue(endsWith(filename, '/test_fopen.txt'));
else
  assert_isequal(filename, [tempdir(), 'test_fopen.txt']);
end
%=============================================================================
[filename, mode] = fopen(ID);
if ismac()
  assert_istrue(endsWith(filename, '/test_fopen.txt'));
else
  assert_isequal(filename, [tempdir(), 'test_fopen.txt']);
end
assert_isequal(mode, 'wb');
%=============================================================================
[filename, mode, machine] = fopen(ID);
if ismac()
  assert_istrue(endsWith(filename, '/test_fopen.txt'));
else
  assert_isequal(filename, [tempdir(), 'test_fopen.txt']);
end
assert_isequal(mode, 'wb');
assert_istrue(strcmp(machine, 'ieee-le') || strcmp(machine, 'ieee-be'));
%=============================================================================
[filename, mode, machine, encoding] = fopen(ID);
if ismac()
  assert_istrue(endsWith(filename, '/test_fopen.txt'));
else
  assert_isequal(filename, [tempdir(), 'test_fopen.txt']);
end
assert_isequal(mode, 'wb');
assert_istrue(strcmp(machine, 'ieee-le') || strcmp(machine, 'ieee-be'));
assert_isequal(encoding, 'UTF-8');
%=============================================================================
fclose(ID);
%=============================================================================
IDS = fopen('all');
assert_isequal(IDS, [0 1 2]);
%=============================================================================
assert_checkerror('fopen()', _('Wrong number of input arguments.'))
%=============================================================================
