%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = vertcat(sparse(zeros(0, 0)), sparse(2));
assert_isequal(R, sparse(2));
R = vertcat(sparse(zeros(1, 0)), sparse(2));
assert_isequal(R, sparse(2));
R = vertcat(sparse(zeros(0, 1)), sparse(2));
assert_isequal(R, sparse(2));
%=============================================================================
