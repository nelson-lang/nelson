%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fwrite'), 3);
asserts.isequal(nargout('fwrite'), -1);
%=============================================================================
TEXT_REF = 'Виртуальная';
skip_testsuite(iswasm(), ...
  'WebAssembly reduced characters_encoding backend does not provide Windows-1251 conversion.');
filename = [tempname(), '.txt'];
F = fopen(filename, 'wt', 'n', 'windows-1251');
W = fwrite(F, TEXT_REF, 'char');
fclose(F);
asserts.isequal(W, 11);
%=============================================================================
F = fopen(filename, 'rt', 'n', 'windows-1251');
[R, S] = fread(F, '*char');
fclose(F);
asserts.isequal(R, TEXT_REF);
rmfile(filename);
%=============================================================================
[R1, R2] = fwrite(stdout(), '和製漢字');
asserts.isequal(R1, 4);
asserts.isequal(R2, 12);
%=============================================================================
