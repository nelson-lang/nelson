%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'a', 'b', '', 'a'});
asserts.isequal(A == 'a', [true false false true]);
asserts.isequal('a' == A, [true false false true]);
asserts.isequal(A ~= 'a', [false true true false]);
asserts.isequal(A == categorical({''}), [false false false false]);
asserts.isequal(A ~= categorical({''}), [true true false true]);
asserts.isequal(A == categorical({'a', 'b', 'c', ''}), [true true false false]);
asserts.checkerror('A == categorical({''a''; ''b''})', sprintf(_('%s must have compatible sizes.'), 'Categorical code arrays'));
%=============================================================================
O = categorical({'low', 'mid', 'high', ''}, {'low', 'mid', 'high'}, ...
  {'low', 'mid', 'high'}, 'Ordinal', true);
asserts.isequal(O < 'mid', [true false false false]);
asserts.isequal(O <= 'mid', [true true false false]);
asserts.isequal(O > 'mid', [false false true false]);
asserts.isequal(O >= 'mid', [false true true false]);
asserts.checkerror('A < ''b''', _('Relational comparisons require ordinal categorical arrays.'));
%=============================================================================
R = categorical({'low'}, {'low', 'high'}, {'low', 'high'}, 'Ordinal', true);
asserts.checkerror('O < R', _('Ordinal categorical arrays must have identical categories.'));
asserts.checkerror('O < R', _('Ordinal categorical arrays must have identical categories.'), 'Nelson:categorical:identicalCategoriesRequired');
M = categorical({'low'}, {'low', 'mid', 'high'}, {'low', 'mid', 'high'}, ...
  'Ordinal', true, 'Protected', true);
asserts.istrue(isequal(R, R));
asserts.isfalse(isequal(R, M));
asserts.istrue(isequaln(A, A));
asserts.isfalse(isequal(A, {'a'}));
%=============================================================================
