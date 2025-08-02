%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('tic'), 0);
assert_isequal(nargout('tic'), 0);
assert_isequal(nargin('toc'), 0);
assert_isequal(nargout('toc'), 1);
%=============================================================================
tic();
sleep(5);
t = toc();
assert_isapprox(t, 5, 1e-1);
tic();
sleep(5)
t = toc();
assert_isapprox(t, 5, 1e-1);
sleep(5)
t = toc();
assert_isapprox(t, 10, 1e-1);
%=============================================================================
