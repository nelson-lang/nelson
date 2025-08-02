%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isinf'), 1)
assert_isequal(nargout('isinf'), 1)
%=============================================================================
assert_isequal(isinf(NaN), false);
assert_isequal(isinf(single(Inf)), true);
assert_isequal(isinf(Inf), true);
%=============================================================================
X = [1 2 Inf 3 -Inf 4];
R = isinf(X);
REF = [ false  false  true   false  true   false];
assert_isequal(R, REF);
%=============================================================================
X = sparse([1 2 Inf 3 0 Inf 0 4]);
R = isinf(X);
I_REF = [1, 1];
J_REF = [3, 6];
V_REF = [true, true];
REF = sparse(I_REF, J_REF, V_REF);
assert_isequal(R, REF);
%=============================================================================
R = isinf([13, Inf, -Inf, NaN]);
REF = logical([0, 1, 1, 0]);
assert_isequal(R, REF);
%=============================================================================
