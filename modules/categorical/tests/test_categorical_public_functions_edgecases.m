%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'a', ''}, {'a', 'b'}, {'a', 'b'}, 'Protected', true);
O = categorical({'low'}, {'low', 'high'}, {'low', 'high'}, 'Ordinal', true);
asserts.istrue(iscategorical(A));
asserts.isfalse(iscategorical({'a'}));
asserts.isfalse(isordinal(A));
asserts.istrue(isordinal(O));
asserts.isfalse(isordinal(1));
asserts.istrue(isprotected(A));
asserts.istrue(isprotected(O));
asserts.isfalse(isprotected('a'));
asserts.isequal(isundefined(A), [false true]);
asserts.isequal(isundefined(ones(2, 3)), false(2, 3));
M = categorical(missing);
asserts.istrue(isundefined(M));
S = categorical(["a" missing ""]);
asserts.isequal(isundefined(S), [false true true]);
%=============================================================================
T = combinations(categorical({'x', 'y'}), categorical({'u', 'v', 'w'}));
asserts.isequal(size(T), [6 2]);
T = combinations([1; 2], {'a', 'b'});
asserts.isequal(size(T), [4 2]);
asserts.checkerror('combinations()', _('Not enough input arguments.'));
%=============================================================================
P = categorical({'a'}, {'a'}, {'a'}, 'Protected', true);
asserts.checkerror('P(1) = ''b'';', _('Cannot add a category to a protected categorical array.'));
asserts.checkerror('categorical({''a''}, ''Ordinal'', true, ''Protected'', false)', ...
  _('Ordinal categorical arrays must be protected.'));
asserts.checkerror('categorical(''abc'')', ...
  _('Character arrays must be wrapped in a cell array or string array.'));
asserts.checkerror('categorical({''a''}, {''a''}, {''good'', ''''})', _('Invalid category name.'));
asserts.checkerror('categorical({''a''}, {''a''}, {''good'', ''''})', _('Invalid category name.'), 'Nelson:categorical:invalidCategoryName');
%=============================================================================
