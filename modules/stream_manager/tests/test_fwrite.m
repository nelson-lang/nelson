%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('fwrite'), 3);
assert_isequal(nargout('fwrite'), 1);
%=============================================================================
TEXT_REF = 'Виртуальная';
filename = [tempdir(), 'fwrite_Windows-1251.txt'];
F = fopen(filename, 'wt', 'n', 'windows-1251');
W = fwrite(F, TEXT_REF, 'char');
fclose(F);
assert_isequal(W, 11);
%=============================================================================
F = fopen(filename, 'rt', 'n', 'windows-1251');
[R, S] = fread(F, '*char');
fclose(F);
assert_isequal(R, TEXT_REF);
%=============================================================================
