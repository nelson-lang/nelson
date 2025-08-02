%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = vertcat(int32(zeros(0, 0)), int32(2));
assert_isequal(R, int32(2));
R = vertcat(int32(zeros(1, 0)), int32(2));
assert_isequal(R, int32(2));
R = vertcat(int32(zeros(0, 1)), int32(2));
assert_isequal(R, int32(2));
%=============================================================================
