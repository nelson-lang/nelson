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
asserts.isequal(class(r), 'char');
asserts.isequal(class(r1), 'char');
asserts.isequal(r, r1);
asserts.istrue(iscellstr(r2))
asserts.isequal(size(r2), [675 1]);
asserts.isequal(size(r1), [1 34776]);
asserts.isequal(size(r1), size(r3));
asserts.isequal(size(r4), [675 1]);
asserts.istrue(isa(r4, 'string'));
%=============================================================================
test_file = [tempname(), '.txt'];
str = 'живете зело, земля, и иже и како люди';
skip_testsuite(iswasm(), ...
  'WebAssembly reduced characters_encoding backend does not provide Windows-1251 conversion.');
filewrite(test_file, str, 'native', 'windows-1251');
T1 = fileread(test_file, 'char', 'native', 'windows-1251');
asserts.isequal(T1, str)
%=============================================================================
T1 = fileread(test_file, 'string', 'native', 'windows-1251');
asserts.isequal(T1, string(str));
%=============================================================================
T2 = fileread(test_file, 'string', 'native', 'auto');
asserts.isequal(T2, string(str));
rmfile(test_file);
%=============================================================================
