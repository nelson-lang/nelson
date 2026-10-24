%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% cell displayed in a struct field: scalars keep their brackets, a nested
% cell is summarized by its size
%=============================================================================
format short
LF = char(10);
X = char(215);
%=============================================================================
A = struct();
A.f = {[1 2], 3, []};
A.d = {'fdfddddd', 'fdd', {'m'}};
A.l = {true, [true false]};
A.i = {int8(5), int8([1 2]), uint16(300)};
A.c = {1 + 2i, [1 + 2i 3]};
A.s = {"abc", ["a" "b"]};
A.m = {missing, string(missing)};
A.e = {zeros(1, 0), '', {}, "", zeros(0, 3), [], cell(1, 0)};
A.g = {{1, [1 2]}};
A.h = {1.5, pi, 1e10, -2};
A.k = {single(2.5), single([1 2.5])};
A.n = {struct('a', 1)};
A.col = {1; 2};
A.sc = {5};
A.x = {'abc'};
A.nan = {NaN, Inf, -Inf, [NaN 1]};
R = evalc('display(A)');
REF = [LF, 'A = ', LF, LF, '  struct with fields:', LF, LF, ...
  '      f: {[1 2]  [3]  []}', LF, ...
  '      d: {''fdfddddd''  ''fdd''  {1', X, '1 cell}}', LF, ...
  '      l: {[1]  [1 0]}', LF, ...
  '      i: {[5]  [1 2]  [300]}', LF, ...
  '      c: {[1.0000 + 2.0000i]  [1.0000 + 2.0000i 3.0000 + 0.0000i]}', LF, ...
  '      s: {["abc"]  ["a"    "b"]}', LF, ...
  '      m: {<missing>  [<missing>]}', LF, ...
  '      e: {[1', X, '0 double]  ''''  {}  [""]  [0', X, '3 double]  []  {1', X, '0 cell}}', LF, ...
  '      g: {{1', X, '2 cell}}', LF, ...
  '      h: {[1.5000]  [3.1416]  [1.0000e+10]  [-2]}', LF, ...
  '      k: {[2.5000]  [1 2.5000]}', LF, ...
  '      n: {[1', X, '1 struct]}', LF, ...
  '    col: {2', X, '1 cell}', LF, ...
  '     sc: {[5]}', LF, ...
  '      x: {''abc''}', LF, ...
  '    nan: {[NaN]  [Inf]  [-Inf]  [NaN 1]}', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
C = {{1, [1 2]}};
R = evalc('display(C)');
REF = [LF, 'C =', LF, LF, '  1', X, '1 cell array', LF, LF, ...
  '    {1', X, '2 cell}', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
B = struct();
B.fh = {@sin};
B.row = {@sin, 1, @cos};
B.sp = {sparse(1), sparse([1 0 2])};
R = evalc('display(B)');
REF = [LF, 'B = ', LF, LF, '  struct with fields:', LF, LF, ...
  '     fh: {[@sin]}', LF, ...
  '    row: {[@sin]  [1]  [@cos]}', LF, ...
  '     sp: {[1]  [1 0 2]}', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
