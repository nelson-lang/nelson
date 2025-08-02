%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
eol_pc = [char(13), char(10)];
eol_unix = char(10);
if ispc()
  eol = eol_pc;
else
  eol = eol_unix;
end
tab = char(9);
%=============================================================================
filename = [tempdir(), 'csvwrite_test.csv'];
%=============================================================================
A = [Inf, -Inf, NaN, 3];
csvwrite(filename, A);
M = csvread(filename);
assert_isequal(A, M);
%=============================================================================
TXT = fileread(filename, 'char', 'native');
REF = ['Inf,-Inf,NaN,3', eol];
assert_isequal(TXT, REF);
%=============================================================================
A = eye(3, 4) + i;
csvwrite(filename, A);
M = csvread(filename);
assert_isequal(A, M);
%=============================================================================
