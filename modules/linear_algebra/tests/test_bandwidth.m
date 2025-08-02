%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('bandwidth'), -1);
assert_isequal(nargout('bandwidth'), -1);
%=============================================================================
assert_isequal(bandwidth([]), 0);
assert_isequal(bandwidth([], 'upper'), 0);
assert_isequal(bandwidth([], 'lower'), 0);
%=============================================================================
assert_isequal(bandwidth(NaN), 0);
assert_isequal(bandwidth(NaN, 'upper'), 0);
assert_isequal(bandwidth(NaN, 'lower'), 0);
%=============================================================================
assert_isequal(bandwidth(Inf), 0);
assert_isequal(bandwidth(Inf, 'upper'), 0);
assert_isequal(bandwidth(Inf, 'lower'), 0);
%=============================================================================
A = tril(magic(6));
R = bandwidth(A, 'lower');
REF = 5;
assert_isequal(R, REF);
%=============================================================================
R = bandwidth(A, 'upper');
REF = 0;
assert_isequal(R, REF);
%=============================================================================