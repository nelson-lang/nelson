%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% cellfun UniformOutput: outputs with a different class must raise an error
asserts.checkerror('cellfun(@(x) subsref({5, ''z''}, substruct(''{}'', {x})), {1, 2})', _('All outputs of the function must be of the same data type when UniformOutput is true.'));
%=============================================================================
R = cellfun(@(x) x * 2, {1, 2});
asserts.isequal(R, [2 4]);
%=============================================================================
% real/complex outputs of the same class are promoted, not rejected
R = cellfun(@sqrt, {4, -4});
asserts.isequal(R, [2, 2i]);
%=============================================================================
% cellfun('isclass', ...): strict class name comparison
R = cellfun('isclass', {1, int8(1)}, 'oub');
asserts.isequal(R, [false false]);
%=============================================================================
R = cellfun('isclass', {1, int8(1)}, 'double');
asserts.isequal(R, [true false]);
%=============================================================================
R = cellfun('isclass', {"s"}, 'char');
asserts.isequal(R, false);
%=============================================================================
R = cellfun('isclass', {"s"}, 'string');
asserts.isequal(R, true);
%=============================================================================
R = cellfun('isclass', {'c'}, 'char');
asserts.isequal(R, true);
%=============================================================================
% cellfun on empty cell: dimensions of the input are preserved
R = cellfun(@(x) x, cell(0, 3));
asserts.isequal(size(R), [0 3]);
asserts.isequal(class(R), 'double');
%=============================================================================
R = cellfun(@(x) x, cell(2, 0));
asserts.isequal(size(R), [2 0]);
%=============================================================================
% cellfun 'UniformOutput' accepts a numeric scalar value
R = cellfun(@(x) x, {1, 2}, 'UniformOutput', 0);
asserts.isequal(R, {1, 2});
%=============================================================================
R = cellfun(@(x) x, {1, 2}, 'UniformOutput', 1);
asserts.isequal(R, [1 2]);
%=============================================================================
% cell2mat with cell of structs
R = cell2mat({struct('a', 1), struct('a', 2)});
asserts.isequal(size(R), [1 2]);
asserts.isequal(R(1).a, 1);
asserts.isequal(R(2).a, 2);
%=============================================================================
% getfield/setfield with a chain of field names
S = struct('a', struct('b', 7));
asserts.isequal(getfield(S, 'a', 'b'), 7);
%=============================================================================
R = setfield(struct('a', struct('b', 0)), 'a', 'b', 5);
asserts.isequal(R.a.b, 5);
%=============================================================================
R = setfield(struct(), 'a', 'b', 5);
asserts.isequal(R.a.b, 5);
%=============================================================================
R = setfield(struct('a', 1), 'a', 2);
asserts.isequal(R.a, 2);
%=============================================================================
asserts.isequal(getfield(struct('x', 3), 'x'), 3);
%=============================================================================
asserts.checkerror('setfield(struct(), 19, 4)', _('Input should be a string or character array.'));
%=============================================================================
% cellstr on N-D char array must error
asserts.checkerror('cellstr(reshape(''abcdefgh'', 2, 2, 2))', sprintf(_('%s must be a 2-D matrix.'), 'Value'));
%=============================================================================
R = cellstr(['ab'; 'cd']);
asserts.isequal(R, {'ab'; 'cd'});
%=============================================================================
