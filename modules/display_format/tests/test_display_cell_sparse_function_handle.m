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
% one-line text of a sparse row or a function handle inside a cell or a struct
%=============================================================================
format short
LF = char(10);
%=============================================================================
% a sparse row reads like its full counterpart
X = {sparse(1)};
asserts.isequal(evalc('disp(X)'), ['    {[1]}', LF, LF]);
X = {sparse([1 2 0])};
asserts.isequal(evalc('disp(X)'), ['    {[1 2 0]}', LF, LF]);
X = {sparse(0)};
asserts.isequal(evalc('disp(X)'), ['    {[0]}', LF, LF]);
X = {sparse(1.5), sparse(2)};
asserts.isequal(evalc('disp(X)'), ['    {[1.5000]}    {[2]}', LF, LF]);
X = {sparse([1.5 2 0])};
asserts.isequal(evalc('disp(X)'), ['    {[1.5000 2 0]}', LF, LF]);
X = {sparse(true)};
asserts.isequal(evalc('disp(X)'), ['    {[1]}', LF, LF]);
X = {sparse([true false])};
asserts.isequal(evalc('disp(X)'), ['    {[1 0]}', LF, LF]);
X = {sparse(1 + 2i)};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000 + 2.0000i]}', LF, LF]);
X = {sparse([1 + 2i 3])};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000 + 2.0000i 3.0000 + 0.0000i]}', LF, LF]);
X = {sparse(1e10)};
asserts.isequal(evalc('disp(X)'), ['    {[1.0000e+10]}', LF, LF]);
X = {sparse(-1.5); 100};
REF = ['    {[-1.5000]}', LF, ...
       '    {[    100]}', LF, LF];
asserts.isequal(evalc('disp(X)'), REF);
X = {sparse(1); 'abcdef'};
REF = ['    {[     1]}', LF, ...
       '    {''abcdef''}', LF, LF];
asserts.isequal(evalc('disp(X)'), REF);
format long
X = {sparse(pi)};
R = evalc('disp(X)');
format short
asserts.isequal(R, ['    {[3.141592653589793]}', LF, LF]);
%=============================================================================
% a sparse matrix, column or empty keeps its size summary
X = {sparse([1 0; 0 2])};
asserts.isequal(evalc('disp(X)'), ['    {2', char(215), '2 double}', LF, LF]);
X = {sparse([1; 2; 3])};
asserts.isequal(evalc('disp(X)'), ['    {3', char(215), '1 double}', LF, LF]);
X = {sparse(zeros(1, 0))};
asserts.isequal(evalc('disp(X)'), ['    {1', char(215), '0 double}', LF, LF]);
%=============================================================================
% a function handle shows its text, right aligned inside the braces
X = {@sin};
asserts.isequal(evalc('disp(X)'), ['    {@sin}', LF, LF]);
X = {@sin, 1, @cos};
asserts.isequal(evalc('disp(X)'), ['    {@sin}    {[1]}    {@cos}', LF, LF]);
X = {['ab'; 'cd']; struct('a', 1); @sin};
REF = ['    {2', char(215), '2 char  }', LF, ...
       '    {1', char(215), '1 struct}', LF, ...
       '    {      @sin}', LF, LF];
asserts.isequal(evalc('disp(X)'), REF);
X = {@sin; 12345678};
REF = ['    {      @sin}', LF, ...
       '    {[12345678]}', LF, LF];
asserts.isequal(evalc('disp(X)'), REF);
X = {@sin, 'a'; 1, @cos};
REF = ['    {@sin}    {''a'' }', LF, ...
       '    {[ 1]}    {@cos}', LF, LF];
asserts.isequal(evalc('disp(X)'), REF);
f = @(x) x + 1;
txt = func2str(f);
X = {f};
asserts.isequal(evalc('disp(X)'), ['    {', txt, '}', LF, LF]);
X = {f; 'abcdefghijklmnopqrst'};
REF = ['    {', blanks(22 - length(txt)), txt, '}', LF, ...
       '    {''abcdefghijklmnopqrst''}', LF, LF];
asserts.isequal(evalc('disp(X)'), REF);
%=============================================================================
% struct fields
s = struct();
s.a = sparse(1);
s.b = sparse([1 2]);
s.c = @sin;
s.d = f;
s.e = sparse(true);
s.f = sparse(1 + 2i);
s.g = sparse([1 0; 0 1]);
s.h = sparse([1 + 2i 3]);
s.i = sparse([1; 2]);
REF = ['    a: 1', LF, ...
       '    b: [1 2]', LF, ...
       '    c: @sin', LF, ...
       '    d: ', txt, LF, ...
       '    e: 1', LF, ...
       '    f: 1.0000 + 2.0000i', LF, ...
       '    g: [2', char(215), '2 double]', LF, ...
       '    h: [1.0000 + 2.0000i 3.0000 + 0.0000i]', LF, ...
       '    i: [2', char(215), '1 double]', LF, LF];
asserts.isequal(evalc('disp(s)'), REF);
%=============================================================================
