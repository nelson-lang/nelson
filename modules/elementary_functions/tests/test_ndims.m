%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ndims'), 1)
assert_isequal(nargout('ndims'), 1)
%=============================================================================
assert_isequal(ndims([]), 2)
assert_isequal(ndims({}), 2)
assert_isequal(ndims(1), 2)
assert_isequal(ndims(1:5), 2)
assert_isequal(ndims(ones(1, 2, 3)), 3)
assert_isequal(ndims(sparse(eye(3, 3))), 2)
%=============================================================================
