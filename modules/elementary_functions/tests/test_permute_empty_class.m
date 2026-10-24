%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An empty array keeps its class once permuted, without trailing singleton
% dimensions.
%=============================================================================
p = permute(cell(1, 0, 2), [3 1 2]);
assert_isequal(class(p), 'cell');
assert_isequal(size(p), [2 1 0]);
p = permute(strings(1, 0, 2), [3 1 2]);
assert_isequal(class(p), 'string');
assert_isequal(size(p), [2 1 0]);
p = permute(int8(zeros(1, 0, 2)), [3 1 2]);
assert_isequal(class(p), 'int8');
assert_isequal(size(p), [2 1 0]);
p = permute(struct('a', cell(1, 0, 2)), [3 1 2]);
assert_isequal(class(p), 'struct');
assert_isequal(fieldnames(p), {'a'});
assert_isequal(size(p), [2 1 0]);
p = permute(true(0, 3), [3 1 2]);
assert_isequal(class(p), 'logical');
assert_isequal(size(p), [1 0 3]);
p = permute(single(zeros(2, 0)), [3 1 2]);
assert_isequal(class(p), 'single');
assert_isequal(size(p), [1 2 0]);
p = permute(zeros(1, 1, 0), [3 1 2]);
assert_isequal(size(p), [0 1]);
assert_isequal(ndims(p), 2);
%=============================================================================
