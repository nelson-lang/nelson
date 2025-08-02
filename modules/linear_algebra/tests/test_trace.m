%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('trace'), 1);
assert_isequal(nargout('trace'), 1);
%=============================================================================
assert_isequal(trace([]), 0);
assert_isequal(trace(NaN), NaN);
assert_isequal(trace(Inf), Inf);
assert_isequal(trace([10, 20; 30, 40]), 50);
assert_isequal(trace([10, 20; 30, 40; 50, 60]), 50);
assert_isequal(trace([10, 30, 50; 20, 40, 60]), 50);
assert_isequal(trace([30:100]), 30);
assert_isequal(trace(eye(1000)), 1000);
%=============================================================================
