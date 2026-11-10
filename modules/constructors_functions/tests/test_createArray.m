%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(createArray(2, 3), zeros(2, 3));
assert_isequal(createArray([2 2]), zeros(2));
assert_isequal(createArray(3), zeros(3));
assert_isequal(class(createArray(2, 'like', single(1))), 'single');
assert_isequal(createArray(1, 3, 'int8'), int8([0 0 0]));
assert_isequal(createArray(2, 2, 'FillValue', 7), 7 * ones(2));
assert_isequal(createArray(1, 2, 'like', int8(1), 'FillValue', 300), int8([127 127]));
R = createArray(1, 2, 'string');
assert_isequal(size(R), [1 2]);
assert_istrue(all(ismissing(R)));
assert_isequal(createArray(1, 2, 'cell'), {[], []});
assert_isequal(createArray(1, 2, 'logical'), [false false]);
assert_isequal(double(createArray(1, 2, 'char')), [0 0]);
assert_isequal(createArray(1, 2, 'FillValue', "x"), ["x" "x"]);
assert_isequal(class(createArray(1, 2, 'FillValue', single(3))), 'single');
assert_isequal(size(createArray(-1, 2)), [0 2]);
assert_isequal(size(createArray(2, 3, 4)), [2 3 4]);
assert_checkerror('createArray(1, 2, ''struct'')', sprintf(_('Class ''%s'' is not supported.'), 'struct'));
%=============================================================================
