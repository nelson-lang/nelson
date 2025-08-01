%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('acoth'), -1);
assert_isequal(nargout('acoth'), 1);
%=============================================================================
R = acoth(-i);
REF = [ 0 + 0.7854i];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
R = acoth(pi);
REF = [0.3298];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
