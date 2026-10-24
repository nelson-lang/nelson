%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('tanh'), 1);
asserts.isequal(nargout('tanh'), 1);
%=============================================================================
asserts.isequal(tanh(NaN), NaN);
asserts.isequal(tanh(-NaN), NaN);
asserts.isequal(tanh(Inf), 1);
asserts.isequal(tanh(-Inf), -1);
%=============================================================================
v = [1. 0.8660254 0.7071068 0.5 0. 0. -0.5 -0.7071068 -0.8660254 -1];
r = tanh(v);
ref = [0.7616 0.6993 0.6089 0.4621 0.0000 0.0000 -0.4621 -0.6089 -0.6993 -0.7616];
asserts.isapprox(r, ref, 1e-4);
%=============================================================================
x = tanh(0i);
ref = 0;
asserts.isequal(x, ref);
asserts.isequal(tanh(0), 0);
asserts.isequal(tanh(-0), 0);
%=============================================================================
X = tanh(zeros(3, 3, 3));
REF = zeros(3, 3, 3);
asserts.isequal(X, REF);
%=============================================================================
S = sparse(zeros(3, 3));
X = tanh(S);
REF = sparse(zeros(3, 3));
asserts.isequal(X, REF);
%=============================================================================
asserts.isequal(tanh([]), []);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'tanh');
asserts.checkerror('tanh(''a'')', msg);
%=============================================================================
