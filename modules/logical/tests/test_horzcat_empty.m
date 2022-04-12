%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = horzcat(logical(zeros(0, 0)), true);
assert_isequal(R, true);
R = horzcat(logical(zeros(1, 0)), true);
assert_isequal(R, true);
R = horzcat(logical(zeros(0, 1)), true);
assert_isequal(R, true);
%=============================================================================
