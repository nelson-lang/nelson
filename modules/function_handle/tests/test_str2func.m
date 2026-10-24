%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
F = str2func('cos');
asserts.isequal(F(.5), feval('cos', .5));
%=============================================================================
F = str2func('@cos')
asserts.isequal(F(.5), feval('cos', .5));
%=============================================================================
F = str2func('asserts.isapprox');
F(1, 1);
[r, msg] = F(1, 2, eps);
asserts.isfalse(r);
asserts.istrue(contains(msg, _('Assertion failed: expected and computed values are too different.')));
%=============================================================================
A = str2func('@(x) x*sqrt(x);');
R = eval('A(4)');
asserts.isequal(R, 8);
B = A(4);
asserts.isequal(B, 8);
%=============================================================================
A = str2func('@(x) x*sqrt(x)');
R = eval('A(4)');
asserts.isequal(R, 8);
B = A(4);
asserts.isequal(B, 8);
%=============================================================================
A = str2func('@(x) disp(x+1)');
A(4)
asserts.checkerror('B = A(4)', _('Too many output arguments.'));
%=============================================================================
rng('default')
b = str2func('@(x, y) find(x > y)')
M = rand(4, 3, 5);
[R, C] = b(M, 0.9);
REF_R = [2 4 1 2 4 1 2 4 4 3 1]';
REF_C = [1 1 3 3 3 4 5 5 6 10 15]';
asserts.isequal(R, REF_R);
asserts.isequal(C, REF_C);
asserts.checkerror('[R, C,d,e] = b(M, 0.9)', _('Too many output arguments.'));
%=============================================================================
F = str2func('@  () 33');
R = evalc('F()');
REF = '
ans =

    33

';
F()
asserts.isequal(R, REF)
B = F();
asserts.isequal(B, 33)
%=============================================================================
str = '@(x) (7*x)-13';
fh = str2func(str);
R = func2str(fh);
REF = '@(x)(7*x)-13';
asserts.isequal(R, REF)
asserts.isequal(fh(3), 8)
%=============================================================================
str = '@(x)7*x-13+a';
fh = str2func(str);
feval('asserts.throws', 'fh(3)', 'a');
%=============================================================================
asserts.checkerror('F = str2func(''@ (y) x= y+1'');', _('A valid function handle expected.'))
%=============================================================================
str = "@(x) (7*x)-13";
fh = str2func(str);
R = func2str(fh);
REF = '@(x)(7*x)-13';
asserts.isequal(R, REF)
%=============================================================================
