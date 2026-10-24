%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('quantile'), -1);
asserts.isequal(nargout('quantile'), 1);
%=============================================================================
A = [2 5 6 10 11 13];
Q = quantile(A, [0.25 0.5 0.75]);
asserts.isequal(Q, [5 8 11]);
asserts.isequal(quantile(A, 3), [5 8 11]);
%=============================================================================
B = [1 NaN 5 10 20];
Q = quantile(B, [0 0.25 0.5 0.75 1]);
asserts.isapprox(Q, [1 3 7.5 15 20], 1e-12);
%=============================================================================
D = reshape(1:30, [3 5 2]);
Qall = quantile(D, [0.25 0.75], 'all');
asserts.isequal(size(Qall), [2 1]);
asserts.isequal(Qall, [8; 23]);
Qpage = quantile(D, [0.25 0.75], [1 2]);
asserts.isequal(size(Qpage), [2 1 2]);
asserts.isapprox(Qpage(:), [4.25; 11.75; 19.25; 26.75], 1e-12);
Qslice = quantile(D, [0.25 0.75], [2 3]);
asserts.isequal(size(Qslice), [3 2]);
asserts.isapprox(Qslice(:), [7; 8; 9; 22; 23; 24], 1e-12);
%=============================================================================
methods = [quantile(1:5, 0.25, 'Method', 'midpoint'), ...
           quantile(1:5, 0.25, 'Method', 'inclusive'), ...
           quantile(1:5, 0.25, 'Method', 'exclusive'), ...
           quantile(1:5, 0.25, 'Method', 'exact'), ...
           quantile(1:5, 0.25, 'Method', 'approximate')];
asserts.isapprox(methods, [1.75 2 1.5 1.75 1.75], 1e-12);
%=============================================================================
S = single([1 2 3]);
Qsingle = quantile(S, [0.25 0.5 0.75]);
asserts.isequal(class(Qsingle), 'single');
asserts.isapprox(double(Qsingle), [1.25 2 2.75], 1e-6);
Qinteger = quantile(int32([1 2 3]), 0.5);
asserts.isequal(class(Qinteger), 'double');
asserts.isequal(Qinteger, 2);
%=============================================================================
asserts.checkerror('quantile()', _('Not enough input arguments.'));
asserts.checkerror('quantile({1}, 0.5)', sprintf(_('%s must be a real numeric array.'), 'A'));
asserts.checkerror('quantile([1 2], -0.1)', sprintf(_('%s must be in the range [0,1].'), 'Probabilities'));
asserts.checkerror('quantile([1 2], -0.1)', sprintf(_('%s must be in the range [0,1].'), 'Probabilities'), 'Nelson:statistics:probabilityOutOfRange');
asserts.checkerror('quantile([1 2], 1.5)', sprintf(_('%s must be in the range [0,1].'), 'Probabilities'));
asserts.checkerror('quantile([1 2], [0.5 NaN])', sprintf(_('%s must be in the range [0,1].'), 'Probabilities'));
asserts.checkerror('quantile([1 2], 0.5, 0)', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('quantile([1 2], 0.5, ''bad'')', _('Invalid quantile option.'));
asserts.checkerror('quantile([1 2], 0.5, ''Method'')', _('Method option requires a value.'));
asserts.checkerror('quantile([1 2], 0.5, ''Method'', ''bad'')', _('Invalid quantile method.'));
%=============================================================================
