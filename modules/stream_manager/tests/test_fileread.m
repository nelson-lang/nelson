%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filesrc = [nelsonroot(), '/gpl-3.0.md'];
r = fileread(filesrc);
r1 = fileread(filesrc, 'char');
r2 = fileread(filesrc, 'cell');
r3 = fileread(filesrc, 'char', 'unix');
r4 = fileread(filesrc, 'string');
assert_isequal(class(r), 'char');
assert_isequal(class(r1), 'char');
assert_isequal(r, r1);
assert_istrue(iscellstr(r2))
assert_isequal(size(r2), [675 1]);
assert_isequal(size(r1), [  1     34776]);
assert_isequal(size(r1), size(r3));
assert_isequal(size(r4), [675 1]);
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
