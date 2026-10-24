%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isnan'), 1)
asserts.isequal(nargout('isnan'), 1)
%=============================================================================
asserts.isequal(isnan(NaN), true);
asserts.isequal(isnan(single(NaN)), true);
asserts.isequal(isnan(Inf), false);
%=============================================================================
X = [1 2 NaN 3 NaN 4];
R = isnan(X);
REF = [false false true false true false];
asserts.isequal(R, REF);
%=============================================================================
X = sparse([1 2 NaN 3 0 NaN 0 4]);
R = isnan(X);
I_REF = [1, 1];
J_REF = [3, 6];
V_REF = [true, true];
REF = sparse(I_REF, J_REF, V_REF, 1, 8);
asserts.isequal(R, REF);
%=============================================================================
R = isnan([13, Inf, NaN, NaN]);
REF = logical([0, 0, 1, 1]);
asserts.isequal(R, REF);
%=============================================================================
asserts.isequal(isnan([1 Inf NaN]), [false false true]);
%=============================================================================
