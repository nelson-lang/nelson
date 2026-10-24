%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(inv(sparse([2 0;0 4])), sparse(reshape([0.5 0 0 0.25], [2 2])));
asserts.isequal(inv(sparse(1)), sparse(1));
asserts.isequal(inv(sparse(0)), sparse(Inf));
asserts.isequal(inv(sparse(eye(3))), sparse(reshape([1 0 0 0 1 0 0 0 1], [3 3])));
asserts.isequal(inv(sparse([])), sparse(zeros([0 0], 'double')));
asserts.isequal(inv(sparse(1+1i)), sparse(complex(0.5, -0.5)));
asserts.isequal(inv(sparse([2 0;0 4i])), sparse(complex(reshape([0.5 0 0 0], [2 2]), reshape([0 0 0 -0.25], [2 2]))));
%=============================================================================
