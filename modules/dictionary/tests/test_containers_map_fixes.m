%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% remove returns the map and removes several keys at once
asserts.isequal(keys(remove(containers.Map({'a', 'b', 'c'}, {1, 2, 3}), {'a', 'c'})), {'b'});
asserts.isequal(keys(remove(containers.Map({'a', 'b'}, {1, 2}), 'a')), {'b'});
asserts.isequal(keys(remove(containers.Map([1 2 3], {1, 2, 3}), {1, 3})), {2});
%=============================================================================
% values(m, keySet): keySet is a cell, the result has its shape
asserts.isequal(values(containers.Map({'a', 'b', 'c', 'd'}, {1, 2, 3, 4}), {'a', 'b'; 'c', 'd'}), {1, 2; 3, 4});
asserts.isequal(size(values(containers.Map('a', 1), {})), [0 0]);
asserts.checkerror('values(containers.Map({''a'', ''b''}, {1, 2}), ''a'')', _('Invalid input for argument 2 (keySet): a cell array of keys expected.'));
%=============================================================================
% vertical concatenation merges the maps, later values override
asserts.isequal(keys([containers.Map('a', 1); containers.Map('b', 2)]), {'a', 'b'});
asserts.isequal(values([containers.Map('a', 1); containers.Map('a', 2)]), {2});
asserts.isequal(keys([containers.Map('a', 1); containers.Map('b', 2); containers.Map('c', 3)]), {'a', 'b', 'c'});
asserts.isequal(keys([containers.Map('a', 1); containers.Map()]), {'a'});
asserts.checkerror('[containers.Map(''a'', 1); containers.Map(1, 2)]', _('All maps must have the same key type to be concatenated.'));
%=============================================================================
% the value type is a class only for char values or scalars of one class
asserts.isequal(containers.Map({'a', 'b'}, {{1}, {2}}).ValueType, 'any');
asserts.isequal(containers.Map({'a', 'b'}, {[1 2], [3 4]}).ValueType, 'any');
asserts.isequal(containers.Map({'a', 'b'}, {struct('a', 1), struct('a', 2)}).ValueType, 'any');
asserts.isequal(containers.Map({'a', 'b'}, {1, 2}).ValueType, 'double');
asserts.isequal(containers.Map({'a', 'b'}, {'y', 'x'}).ValueType, 'char');
asserts.isequal(containers.Map({'a', 'b'}, {true, false}).ValueType, 'logical');
%=============================================================================
% a single text key takes the whole value
asserts.isequal(values(containers.Map('a', [1 2])), {[1 2]});
asserts.isequal(double(containers.Map('a', [1 2]).Count), 1);
asserts.isequal(values(containers.Map('a', {{5, 6}})), {{{5, 6}}});
%=============================================================================
% invalid key and value sets
asserts.checkerror('containers.Map(NaN, 1)', _('NaN-valued key is not allowed for this container.'));
asserts.checkerror('containers.Map({1, ''a''}, {''c'', ''a''})', _('Specified key type does not match the type expected for this container.'));
asserts.checkerror('containers.Map({''a'', ''b''}, {1})', _('The number of keys and values must match.'));
asserts.checkerror('containers.Map({''a'', ''b''}, {1, ''x''}, ''UniformValues'', true)', _('When ''UniformValues'' is true, values must be all character arrays or all scalars of matching type.'));
asserts.isfalse(isKey(containers.Map([1 2], {1, 2}), 'a'));
m = containers.Map('KeyType', 'double', 'ValueType', 'any');
asserts.checkerror('m(''a'') = 1', _('Specified key type does not match the type expected for this container.'));
%=============================================================================
