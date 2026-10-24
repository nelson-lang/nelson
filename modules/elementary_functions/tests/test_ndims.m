%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ndims'), 1)
asserts.isequal(nargout('ndims'), 1)
%=============================================================================
asserts.isequal(ndims([]), 2)
asserts.isequal(ndims({}), 2)
asserts.isequal(ndims(1), 2)
asserts.isequal(ndims(1:5), 2)
asserts.isequal(ndims(ones(1, 2, 3)), 3)
asserts.isequal(ndims(sparse(eye(3, 3))), 2)
%=============================================================================
asserts.isequal(ndims(reshape(1:6, 2, 3)), 2);
asserts.isequal(ndims(reshape([], 0, 3)), 2);
%=============================================================================
% ndims of a classdef/handle object defaults to 2 (no ndims overload)
m = containers.Map('a', 1);
assert_isequal(ndims(m), 2);
%=============================================================================
