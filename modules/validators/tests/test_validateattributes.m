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
addpath(modulepath('validators', 'tests'));
%=============================================================================
asserts.isequal(nargin('validateattributes'), -3);
asserts.isequal(nargout('validateattributes'), 0);
%=============================================================================
validateattributes([1 2 3], {'numeric'}, {'row', 'vector', 'nonempty', 'positive'});
validateattributes([1; 2; 3], {'double'}, {'column', 'increasing'});
validateattributes([1 1; 2 2], {'numeric'}, {'2d', 'nondecreasing', 'nrows', 2, 'ncols', 2});
validateattributes(eye(3), {'double'}, {'square', 'diag', 'size', [3 3]});
validateattributes(uint8([1 2 3]), {'numeric'}, {'integer', 'finite', '>=', 1, '<=', 3});
validateattributes(int16([-2 -1]), {'integer'}, {'negative'});
validateattributes(single([1 2]), {'float'}, {'finite'});
validateattributes(logical([true false]), {'logical'}, {'binary'});
validateattributes("abc", {'string'}, {'scalartext'});
validateattributes('abc', {'char'}, {'row', 'scalartext', 'numel', 3});
validateattributes(ones(2, 3, 4), {'numeric'}, {'3d', 'size', [2 NaN 4], 'ndims', 3});
validateattributes([4; 3; 2], {'numeric'}, {'decreasing', 'nonincreasing'});
validateattributes([2 4 6], {'numeric'}, {'even', 'integer'});
validateattributes([1 3 5], {'numeric'}, {'odd', 'integer'});
validateattributes([-3 -2 -1], {'numeric'}, {'negative', '<', 0});
validateattributes([0 1 2], {'numeric'}, {'nonnegative'});
validateattributes([1 2 3], {'numeric'}, {'nonzero'});
validateattributes(single([1 2]), {'single', 'double'}, {'finite'});
validateattributes(struct('a', 1), {'struct'}, {'scalar'});
%=============================================================================
assertErrorContains(@() validateattributes([1 2], {'char'}, {}), 'types');
assertErrorContains(@() validateattributes(1.5, {'integer'}, {}), 'types');
assertErrorContains(@() validateattributes(uint8(1), {'float'}, {}), 'types');
assertErrorContains(@() validateattributes([1 2], {'numeric'}, {'column'}), 'column');
assertErrorContains(@() validateattributes([1 NaN], {'numeric'}, {'nonnan'}), 'NaN');
assertErrorContains(@() validateattributes([2; 2], {'numeric'}, {'increasing'}), 'monotonicity');
assertErrorContains(@() validateattributes(1 + i, {'numeric'}, {'positive'}, 'demo', 'value', 1), 'positive');
assertErrorContains(@() validateattributes(ones(2, 2), {'numeric'}, {'size', [2 3]}), 'size');
assertErrorContains(@() validateattributes(ones(2, 2), {'numeric'}, {'numel', 3}), 'elements');
assertErrorContains(@() validateattributes(ones(2, 2), {'numeric'}, {'nrows', 3}), 'rows');
assertErrorContains(@() validateattributes(ones(2, 2), {'numeric'}, {'ncols', 3}), 'columns');
assertErrorContains(@() validateattributes(ones(2, 2), {'numeric'}, {'ndims', 3}), 'dimensions');
assertErrorContains(@() validateattributes(ones(2, 2), {'numeric'}, {'scalar'}), 'scalar');
assertErrorContains(@() validateattributes(ones(2, 3), {'numeric'}, {'square'}), 'square');
assertErrorContains(@() validateattributes([1 1; 0 1], {'numeric'}, {'diag'}), 'diagonal');
assertErrorContains(@() validateattributes([], {'numeric'}, {'nonempty'}), 'nonempty');
assertErrorContains(@() validateattributes(sparse(eye(2)), {'numeric'}, {'nonsparse'}), 'nonsparse');
assertErrorContains(@() validateattributes([1 Inf], {'numeric'}, {'finite'}), 'finite');
assertErrorContains(@() validateattributes([0 2], {'numeric'}, {'binary'}), 'binary');
assertErrorContains(@() validateattributes([1 2], {'numeric'}, {'odd'}), 'odd');
assertErrorContains(@() validateattributes([1.5 2], {'numeric'}, {'integer'}), 'integer');
assertErrorContains(@() validateattributes(1 + i, {'numeric'}, {'real'}), 'real');
assertErrorContains(@() validateattributes([-1 0], {'numeric'}, {'positive'}), 'positive');
assertErrorContains(@() validateattributes([0 1], {'numeric'}, {'nonzero'}), 'nonzero');
assertErrorContains(@() validateattributes([1 2], {'numeric'}, {'>', 1}), 'range');
assertErrorContains(@() validateattributes([1 2], {'numeric'}, {'unknown'}), 'Unknown');
assertErrorContains(@() validateattributes([1 2], {'numeric'}, {'size'}), 'requires');
validateattributes([1 2], 'numeric', 'row');
assertErrorContains(@() validateattributes([1; 2], 'numeric', 'row'), 'row');
assertErrorIdentifier(@() validateattributes([1 2], {'char'}, {}), 'Nelson:validators:invalidType');
assertErrorIdentifier(@() validateattributes([1 2], {'numeric'}, {'column'}), 'Nelson:expectedColumn');
assertErrorIdentifier(@() validateattributes([1 2], {'numeric'}, {'>', 1}), 'Nelson:notGreater');
% with a function name the identifier is prefixed by it, using the established convention
assertErrorIdentifier(@() validateattributes(-1, {'numeric'}, {'positive'}, 'myfun', 'X', 2), 'Nelson:myfun:expectedPositive');
assertErrorIdentifier(@() validateattributes([1 2], {'char'}, {}, 'myfun'), 'Nelson:myfun:invalidType');
assertErrorIdentifier(@() validateattributes([1 2], {'numeric'}, {'>', 1}, 'myfun'), 'Nelson:myfun:notGreater');
%=============================================================================
