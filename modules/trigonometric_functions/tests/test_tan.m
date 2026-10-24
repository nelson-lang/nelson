%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('tan'), 1);
asserts.isequal(nargout('tan'), 1);
%=============================================================================
asserts.isequal(tan(NaN), NaN);
asserts.isequal(tan(-NaN), NaN);
asserts.isequal(tan(Inf), NaN);
asserts.isequal(tan(-Inf), NaN);
%=============================================================================
v = [1. 0.8660254 0.7071068 0.5 0. 0. -0.5 -0.7071068 -0.8660254 -1];
r = tan(v);
ref = [1.5574 1.1758 0.8545 0.5463 0.0000 0.0000 -0.5463 -0.8545 -1.1758 -1.5574];
asserts.isapprox(r, ref, 1e-4);
%=============================================================================
x = tan(0i);
ref = 0;
asserts.isequal(x, ref);
asserts.isequal(tan(0), 0);
asserts.isequal(tan(-0), 0);
%=============================================================================
X = tan(zeros(3, 3, 3));
REF = zeros(3, 3, 3);
asserts.isequal(X, REF);
%=============================================================================
S = sparse(zeros(3, 3));
X = tan(S);
REF = sparse(zeros(3, 3));
asserts.isequal(X, REF);
%=============================================================================
asserts.isequal(tan([]), []);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'tan');
asserts.checkerror('tan(''a'')', msg);
%=============================================================================
