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
filesrc = [nelsonroot(), '/COPYING'];
r = fileread(filesrc);
r1 = fileread(filesrc, 'char');
r2 = fileread(filesrc, 'cell');
r3 = fileread(filesrc, 'char', 'unix');
r4 = fileread(filesrc, 'string');
assert_isequal(class(r), 'char');
assert_isequal(class(r1), 'char');
assert_isequal(r, r1);
assert_istrue(iscellstr(r2))
assert_isequal(size(r2), [339 1]);
assert_isequal(size(r1), [  1     18092]);
assert_isequal(size(r1), size(r3));
assert_isequal(size(r4), [339 1]);
assert_istrue(isa(r4, 'string'));
%=============================================================================
str = 'живете зело, земля, и иже и како люди';
filewrite([tempdir(), 'test_fileread.txt'], str, 'native', 'windows-1251');
T1 = fileread([tempdir(), 'test_fileread.txt'], 'char', 'native', 'windows-1251');
assert_isequal(T1, str)
%=============================================================================
T1 = fileread([tempdir(), 'test_fileread.txt'], 'string', 'native', 'windows-1251');
assert_isequal(T1, string(str));
%=============================================================================
T2 = fileread([tempdir(), 'test_fileread.txt'], 'string', 'native', 'auto');
assert_isequal(T2, string(str));
%=============================================================================
