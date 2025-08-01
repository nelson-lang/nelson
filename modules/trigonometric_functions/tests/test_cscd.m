%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cscd'), -1);
assert_isequal(nargout('cscd'), 1);
%=============================================================================
R = cscd([35+i, 15+2i, 10+3i]);
REF = [ 1.7421 - 0.0434i,   3.7970 - 0.4944i,   5.2857 - 1.5681i];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
