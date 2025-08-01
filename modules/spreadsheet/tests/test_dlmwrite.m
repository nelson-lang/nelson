%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('dlmwrite'), -3);
assert_isequal(nargout('dlmwrite'), 0);
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
filename = [tempdir(), 'dlmwrite_test.csv'];
%=============================================================================
% dlmwrite(filename, M)
A = [Inf, -Inf, NaN, 3];
dlmwrite(filename, A);
R = fileread(filename, 'char', 'native');
REF = ['Inf,-Inf,NaN,3', eol];
assert_isequal(R, REF);
%=============================================================================
A = [];
dlmwrite(filename, A);
R = fileread(filename, 'char', 'native');
REF = '';
assert_isequal(R, REF);
%=============================================================================
A = eye(3, 4);
dlmwrite(filename, A);
R = fileread(filename, 'char', 'native');
REF = ['1,0,0,0', eol, '0,1,0,0', eol, '0,0,1,0', eol];
assert_isequal(R, REF);
%=============================================================================
A = eye(3, 4) + i;
dlmwrite(filename, A);
R = fileread(filename, 'char', 'native');
REF = ['1+1i,0+1i,0+1i,0+1i', eol, '0+1i,1+1i,0+1i,0+1i', eol, '0+1i,0+1i,1+1i,0+1i', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, delimiter)
A = eye(3, 4) + i;
dlmwrite(filename, A, '\t');
R = fileread(filename, 'char', 'native');
REF = ['1+1i', tab, '0+1i', tab, '0+1i', tab, '0+1i', eol, '0+1i', tab, '1+1i', tab, '0+1i', tab, '0+1i', eol, '0+1i', tab, '0+1i', tab, '1+1i', tab, '0+1i', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, '-append')
A = 1;
dlmwrite(filename, A);
B = 2;
dlmwrite(filename, B, '-append');
R = fileread(filename, 'char', 'native');
REF = ['1', eol, '2', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, '-append', delimiter)
A = [1, 2];
dlmwrite(filename, A);
B = [3, 4];
dlmwrite(filename, B, '-append', '|');
R = fileread(filename, 'char', 'native');
REF = ['1,2', eol, '3|4', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, delimiter, r, c)
A = reshape([1:27], 3, 3, 3);
dlmwrite(filename, A, ';', 5, 7);
R = fileread(filename, 'char', 'native');
REF = [';;;;;;;;;;;;;;;', eol, ';;;;;;;;;;;;;;;', eol, ';;;;;;;;;;;;;;;', eol, ';;;;;;;;;;;;;;;', eol, ';;;;;;;;;;;;;;;', eol, ';;;;;;;1;4;7;10;13;16;19;22;25', eol, ';;;;;;;2;5;8;11;14;17;20;23;26', eol, ';;;;;;;3;6;9;12;15;18;21;24;27', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, delimiter, r, c)
A = eye(2, 2);
dlmwrite(filename, A, ';', 2, 2);
R = fileread(filename, 'char', 'native');
REF = [';;;', eol, ';;;', eol, ';;1;0', eol, ';;0;1', eol];
assert_isequal(R, REF);
%=============================================================================
A = eye(3, 2);
dlmwrite(filename, A, ';', 4, -5);
R = fileread(filename, 'char', 'native');
REF = [eol, eol, eol, eol, '1;0', eol, '0;1', eol, '0;0', eol];
assert_isequal(R, REF);
%=============================================================================
A = eye(3, 2);
dlmwrite(filename, A, ';', 4, 5);
R = fileread(filename, 'char', 'native');
REF = [';;;;;;', eol, ';;;;;;', eol, ';;;;;;', eol, ';;;;;;', eol, ';;;;;1;0', eol, ';;;;;0;1', eol, ';;;;;0;0', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, delimiter, r, c, eol)
A = eye(3, 2);
dlmwrite(filename, A, ';', 0, 0, 'pc');
R = fileread(filename, 'char', 'pc');
REF = ['1;0', eol_pc, '0;1', eol_pc, '0;0', eol_pc];
assert_isequal(R, REF);
%=============================================================================
A = eye(3, 2);
dlmwrite(filename, A, ';', 0, 0, 'unix');
R = fileread(filename, 'char', 'unix');
REF = ['1;0', eol_unix, '0;1', eol_unix, '0;0', eol_unix];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, delimiter, r, c, eol, precision)
A = [pi, pi];
dlmwrite(filename, A, ';', 0, 0, 'unix', 2);
R = fileread(filename, 'char', 'unix');
REF = ['3.1;3.1', eol_unix];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, delimiter, r, c, eol, precision)
A = [pi, pi];
dlmwrite(filename, A, ';', 0, 0, 'unix', '%10.3f');
R = fileread(filename, 'char', 'unix');
REF = ['     3.142;     3.142', eol_unix];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, '-append', delimiter, r, c)
A = 1;
dlmwrite(filename, A);
B = 2;
dlmwrite(filename, B, '-append', ';', 1, 1);
R = fileread(filename, 'char', 'native');
REF = ['1', eol, ';', eol, ';2', eol];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, '-append', delimiter, r, c, eol)
A = 1;
dlmwrite(filename, A, ',', 0, 0, 'pc');
B = 2;
dlmwrite(filename, B, '-append', ';', 1, 1, 'pc');
R = fileread(filename, 'char', 'pc');
REF = ['1', eol_pc, ';', eol_pc, ';2', eol_pc];
assert_isequal(R, REF);
%=============================================================================
% dlmwrite(filename, M, '-append', delimiter, r, c, eol, precision)
A = 1;
dlmwrite(filename, A, ',', 0, 0, 'pc');
B = 2;
dlmwrite(filename, B, '-append', ';', 1, 1, 'pc', 5);
R = fileread(filename, 'char', 'pc');
REF = ['1', eol_pc, ';', eol_pc, ';2', eol_pc];
assert_isequal(R, REF);
%=============================================================================
A = reshape([1:27], 3, 3, 3);
dlmwrite(filename, A);
R = fileread(filename, 'char', 'native');
REF = ['1,4,7,10,13,16,19,22,25', eol, '2,5,8,11,14,17,20,23,26', eol, '3,6,9,12,15,18,21,24,27', eol];
assert_isequal(R, REF);
%=============================================================================
A = reshape([1:40], 4, 2, []);
dlmwrite(filename, A);
R = fileread(filename, 'char', 'native');
REF = ['1,5,9,13,17,21,25,29,33,37', eol, '2,6,10,14,18,22,26,30,34,38', eol, '3,7,11,15,19,23,27,31,35,39', eol, '4,8,12,16,20,24,28,32,36,40', eol];;
assert_isequal(R, REF);
%=============================================================================
