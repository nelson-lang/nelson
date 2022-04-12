%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = horzcat(single(zeros(0, 0)), single(2));
assert_isequal(R, single(2));
R = horzcat(single(zeros(1, 0)), single(2));
assert_isequal(R, single(2));
R = horzcat(single(zeros(0, 1)), single(2));
assert_isequal(R, single(2));
%=============================================================================
