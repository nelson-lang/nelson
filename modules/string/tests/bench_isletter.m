%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = blanks(1e8);
r(5) = 't';
tic();R = isletter(r);toc()
assert_isequal(R(5), true);
assert_isequal(R(4), false);