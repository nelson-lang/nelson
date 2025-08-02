%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
