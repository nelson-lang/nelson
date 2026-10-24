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
% one-line text of a cell element: one blank between elements, each element
% in its own short format, logical as 1/0, entries aligned inside the braces
%=============================================================================
format short
LF = char(10);
%=============================================================================
C = {[1 2]; true; int8(5); 3.25; -1; [1.5 2]};
R = evalc('disp(C)');
REF = ['    {[     1 2]}', LF, ...
       '    {[       1]}', LF, ...
       '    {[       5]}', LF, ...
       '    {[  3.2500]}', LF, ...
       '    {[      -1]}', LF, ...
       '    {[1.5000 2]}', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
X = {[true false true]};
asserts.isequal(evalc('disp(X)'), ['    {[1 0 1]}', LF, LF]);
X = {int8([1 -2 30])};
asserts.isequal(evalc('disp(X)'), ['    {[1 -2 30]}', LF, LF]);
X = {uint16([100 2])};
asserts.isequal(evalc('disp(X)'), ['    {[100 2]}', LF, LF]);
X = {[NaN Inf -Inf]};
asserts.isequal(evalc('disp(X)'), ['    {[NaN Inf -Inf]}', LF, LF]);
X = {[1e10 1]};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000e+10 1]}', LF, LF]);
X = {[0.001 1]};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000e-03 1]}', LF, LF]);
X = {[12345.6 1]};
asserts.isequal(evalc('disp(X)'), ['    {[1.2346e+04 1]}', LF, LF]);
X = {single([1 2.5])};
asserts.isequal(evalc('disp(X)'), ['    {[1 2.5000]}', LF, LF]);
X = {false};
asserts.isequal(evalc('disp(X)'), ['    {[0]}', LF, LF]);
%=============================================================================
X = {1 + 2i};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000 + 2.0000i]}', LF, LF]);
X = {[1 + 2i 3 - 4.5i]};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000 + 2.0000i 3.0000 - 4.5000i]}', LF, LF]);
%=============================================================================
X = {""};
asserts.isequal(evalc('disp(X)'), ['    {[""]}', LF, LF]);
X = {string(missing)};
asserts.isequal(evalc('disp(X)'), ['    {[<missing>]}', LF, LF]);
X = {"x"};
asserts.isequal(evalc('disp(X)'), ['    {["x"]}', LF, LF]);
%=============================================================================
X = {zeros(1, 0)};
asserts.isequal(evalc('disp(X)'), ['    {1', char(215), '0 double}', LF, LF]);
X = {''};
asserts.isequal(evalc('disp(X)'), ['    {0', char(215), '0 char}', LF, LF]);
X = {['ab'; 'cd']};
asserts.isequal(evalc('disp(X)'), ['    {2', char(215), '2 char}', LF, LF]);
X = {struct('a', 1)};
asserts.isequal(evalc('disp(X)'), ['    {1', char(215), '1 struct}', LF, LF]);
%=============================================================================
A = {'abc'; [1 2]; "xy"; [1; 2]; true; {1}};
R = evalc('disp(A)');
REF = ['    {''abc''     }', LF, ...
       '    {[     1 2]}', LF, ...
       '    {["xy"    ]}', LF, ...
       '    {2', char(215), '1 double}', LF, ...
       '    {[       1]}', LF, ...
       '    {1', char(215), '1 cell  }', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
M = {1 + 2i; -1 - 1i; 10};
R = evalc('disp(M)');
REF = ['    {[ 1.0000 + 2.0000i]}', LF, ...
       '    {[-1.0000 - 1.0000i]}', LF, ...
       '    {[               10]}', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
format long
X = {[1.5 2]};
asserts.isequal(evalc('disp(X)'), ['    {[1.500000000000000 2]}', LF, LF]);
X = {single(pi)};
asserts.isequal(evalc('disp(X)'), ['    {[3.1415927]}', LF, LF]);
format short
%=============================================================================
s = struct();
s.b = true;
s.d = int8([1 2]);
s.x = [1 + 2i 3 - 4.5i];
s.z = zeros(1, 0);
R = evalc('s');
REF = [LF, 's = ', LF, LF, ...
       '  struct with fields:', LF, LF, ...
       '    b: 1', LF, ...
       '    d: [1 2]', LF, ...
       '    x: [1.0000 + 2.0000i 3.0000 - 4.5000i]', LF, ...
       '    z: [1', char(215), '0 double]', LF, LF];
asserts.isequal(R, REF);
%=============================================================================
