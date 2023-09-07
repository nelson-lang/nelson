%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('atanh'), 1);
assert_isequal(nargout('atanh'), 1);
%=============================================================================
assert_isequal(atanh(NaN),  NaN);
assert_isequal(atanh(-NaN), NaN);
assert_isequal(atanh(Inf), complex(0,pi * 0.5));
assert_isequal(atanh(-Inf), complex(0,-pi * 0.5));
%=============================================================================
v = [1.   0.8660254   0.7071068   0.5   0.   0.  -0.5  -0.7071068  -0.8660254  -1];
r = atanh(v);
ref = [Inf,  1.3170, 0.8814, 0.5493, 0, 0, -0.5493, -0.8814, -1.3170, -Inf];
assert_isapprox(real(r), ref, 1e-4);
%=============================================================================
x = atanh(0i);
ref = 0;
assert_isequal(x, ref);
%=============================================================================
X = atanh(zeros(3, 3, 3));
REF = zeros(3, 3, 3);
assert_isequal(X, REF);
%=============================================================================
R = atanh([12, NaN]);
REF = [0.0835+1.5708i, complex(NaN, 0)];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
assert_isequal(atanh([]), []);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'atanh');
assert_checkerror('atanh(''a'')', msg);
%=============================================================================
