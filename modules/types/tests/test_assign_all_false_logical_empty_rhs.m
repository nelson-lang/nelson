%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% An all-false logical index selects zero elements, so an empty right-hand
% side must be a no-op for any left-hand side class (not only double).
%=============================================================================
m = logical([0 0 0]);
%=============================================================================
% single left-hand side, empty single right-hand side (self index)
y = single([0.3 0.7 1.5]);
y(m) = y(m);
asserts.isequal(class(y), 'single');
asserts.isequal(y, single([0.3 0.7 1.5]));
%=============================================================================
% single left-hand side, explicit empty single right-hand side
y = single([0.3 0.7 1.5]);
y(m) = single([]);
asserts.isequal(class(y), 'single');
asserts.isequal(y, single([0.3 0.7 1.5]));
%=============================================================================
% single left-hand side, empty double right-hand side
y = single([0.3 0.7 1.5]);
y(m) = [];
asserts.isequal(class(y), 'single');
asserts.isequal(y, single([0.3 0.7 1.5]));
%=============================================================================
% int8 left-hand side
z = int8([1 2 3]);
z(m) = z(m);
asserts.isequal(class(z), 'int8');
asserts.isequal(z, int8([1 2 3]));
%=============================================================================
% int8 left-hand side, explicit empty int8 right-hand side
z = int8([1 2 3]);
z(m) = int8([]);
asserts.isequal(class(z), 'int8');
asserts.isequal(z, int8([1 2 3]));
%=============================================================================
% logical left-hand side
b = logical([1 0 1]);
b(m) = b(m);
asserts.isequal(class(b), 'logical');
asserts.isequal(b, logical([1 0 1]));
%=============================================================================
% double left-hand side (baseline, was already a no-op)
d = [0.3 0.7 1.5];
d(m) = d(m);
asserts.isequal(class(d), 'double');
asserts.isequal(d, [0.3 0.7 1.5]);
%=============================================================================
% a genuine deletion through a non-empty selection with a non-double empty
% right-hand side must still be rejected (regression guard).
y = single([0.3 0.7 1.5]);
asserts.checkerror('y(2) = single([])', _('Empty matrix of type double expected.'));
%=============================================================================
% same behavior with a two-subscript index whose logical subscript is
% all-false: the whole selection is empty, so it is a no-op for any class.
rows = logical([0 0]);
%=============================================================================
Y = single([1 2 3; 4 5 6]);
Y(rows, 2) = Y(rows, 2);
asserts.isequal(class(Y), 'single');
asserts.isequal(Y, single([1 2 3; 4 5 6]));
%=============================================================================
Y = single([1 2 3; 4 5 6]);
Y(rows, :) = single([]);
asserts.isequal(class(Y), 'single');
asserts.isequal(Y, single([1 2 3; 4 5 6]));
%=============================================================================
Y = single([1 2 3; 4 5 6]);
Y(rows, :) = [];
asserts.isequal(class(Y), 'single');
asserts.isequal(size(Y), [2 3]);
asserts.isequal(Y, single([1 2 3; 4 5 6]));
%=============================================================================
Z = int8([1 2 3; 4 5 6]);
Z(rows, :) = int8([]);
asserts.isequal(class(Z), 'int8');
asserts.isequal(Z, int8([1 2 3; 4 5 6]));
%=============================================================================
% logical column mask with a non-empty scalar right-hand side
A = [1 2 3];
A(:, logical([0 0 0])) = 0;
asserts.isequal(A, [1 2 3]);
%=============================================================================
B = [1 2 3 4];
B(:, logical([0 1 0 1])) = 9;
asserts.isequal(B, [1 9 3 9]);
%=============================================================================
C = magic(3);
C(logical([1 0 1]), :) = 0;
asserts.isequal(C, [0 0 0; 3 5 7; 0 0 0]);
%=============================================================================
D = [10 20 30];
D(logical([0 1 1])) = [-1 -2];
asserts.isequal(D, [10 -1 -2]);
%=============================================================================
