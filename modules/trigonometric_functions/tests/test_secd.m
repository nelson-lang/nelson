%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('secd'), -1);
assert_isequal(nargout('secd'), 1);
%=============================================================================
R = secd([1, 10+3i, 15+2i, 35+i]);
REF = [1.0002 + 0.0000i,   1.0140 + 0.0094i,   1.0346 + 0.0097i,   1.2204 + 0.0149i];
assert_isapprox(real(R), real(REF), 1e-4);
assert_isapprox(imag(R), imag(REF), 1e-2);
%=============================================================================
