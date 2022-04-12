%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = vertcat(char(zeros(0, 0)), 'A');
assert_isequal(R, 'A');
R = vertcat(char(zeros(1, 0)), 'A');
assert_isequal(R, 'A');
R = vertcat(char(zeros(0, 1)), 'A');
assert_isequal(R, 'A');
%=============================================================================
