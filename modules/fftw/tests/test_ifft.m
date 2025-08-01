%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ifft'), 3);
assert_isequal(nargout('ifft'), 1);
%=============================================================================
X = [1:5];
Y = fft(X);
II = ifft(Y);
assert_isapprox(II, X, eps);
%=============================================================================
Y = ifft(ones(3,2), 3, 1);
REF = [1, 1; 0, 0; 0, 0];
assert_isequal(Y, REF);
%=============================================================================
