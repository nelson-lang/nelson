%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ifftn'), -1);
assert_isequal(nargout('ifftn'), 1);
%=============================================================================
f = zeros(5, 5);
f(1:5,4:5) = 1;
Y = ifftn(fftn(f));
assert_isapprox(Y, f, 1e-4);
%=============================================================================
