%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isfinite'), 1)
assert_isequal(nargout('isfinite'), 1)
%=============================================================================
assert_isequal(isfinite(NaN), false);
assert_isequal(isfinite(single(Inf)), false);
assert_isequal(isfinite(Inf), false);
%=============================================================================
X = [1 2 Inf 3 -Inf 4];
R = isfinite(X);
REF = [ true  true  false   true  false   true];
assert_isequal(R, REF);
%=============================================================================
X = sparse([1 2 Inf 3 0 Inf 0 4]);
R = isfinite(X);
REF = [ true   true   false  true   true   false  true   true ];
assert_isequal(R, REF);
%=============================================================================
R = isfinite([13, Inf, -Inf, NaN]);
REF = logical([1, 0, 0, 0]);
assert_isequal(R, REF);
%=============================================================================
