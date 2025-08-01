%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('log'), 1)
assert_isequal(nargout('log'), 1)
%=============================================================================
R = log(-1);
REF = complex(0, pi);
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = log(3);
REF = 1.0986;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = log(3+i);
REF = 1.1513 + 0.3218i;
assert_isapprox(real(R), real(REF), 1e-3);
assert_isapprox(imag(R), imag(REF), 1e-3);
%=============================================================================
R = log(single(-1));
REF = single(complex(0, pi));
assert_isapprox(R, REF, 1e-3);
%=============================================================================
R = log(single(3+i));
REF = single(1.1513 + 0.3218i);
assert_isapprox(real(R), real(REF), 1e-3);
assert_isapprox(imag(R), imag(REF), 1e-3);
%=============================================================================
R = log(eye(3,2));
REF = [0  -Inf;
-Inf     0;
-Inf  -Inf];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = log(NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
