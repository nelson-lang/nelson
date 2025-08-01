%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('asech'), -1);
assert_isequal(nargout('asech'), 1);
%=============================================================================
R = asech([2, -3, 1+2i]);
REF = [0.0000 + 1.0472i,   0.0000 + 1.9106i,   0.3966 - 1.3845i];
assert_isapprox(abs(real(R)), abs(real(REF)), 1e-4);
assert_isapprox(abs(imag(R)), abs(imag(REF)), 1e-4);
%=============================================================================
