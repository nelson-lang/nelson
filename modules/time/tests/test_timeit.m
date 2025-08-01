%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('timeit'), -2);
assert_isequal(nargout('timeit'), 1);
%=============================================================================
f = str2func('@()sleep(6)');
t = timeit(f);
assert_isapprox(t, 6, 1e-1);
%=============================================================================
