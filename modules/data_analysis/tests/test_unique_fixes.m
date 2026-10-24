%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(unique(sparse([3 0 0 1])), sparse([0 1 3]));
asserts.isequal(unique(sparse([3 0;0 1])), sparse(reshape([0 1 3], [3 1])));
asserts.isequal(unique(sparse([3 0 0 1]),'stable'), sparse([3 0 1]));
asserts.isequal(unique(sparse([1 2;3 4]),'rows'), sparse(reshape([1 3 2 4], [2 2])));
asserts.isequal(unique(sparse([])), sparse(zeros([0 1], 'double')));
asserts.isequal(unique(sparse(0,3)), sparse(zeros([0 1], 'double')));
asserts.isequal(unique(sparse(2,2)), sparse(0));
asserts.isequal(unique(sparse([1 1 2 2]')), sparse(reshape([1 2], [2 1])));
asserts.isequal(unique(sparse([1 0 1 0]),'last'), sparse([0 1]));
asserts.isequal(unique(sparse([1 0 1 0]),'first'), sparse([0 1]));
asserts.isequal(unique(sparse([1 0 1 0]),'sorted'), sparse([0 1]));
asserts.isequal(unique(sparse([1 0;1 0]),'rows'), sparse([1 0]));
asserts.isequal(unique(sparse(1+1i)), sparse(complex(1, 1)));
asserts.isequal(unique(sparse([true false true])), sparse(logical([0 1])));
asserts.isequal(unique(sparse(5)), sparse(5));
asserts.isequal(unique(sparse([NaN NaN 1])), sparse([1 NaN NaN]));
asserts.isequal(unique(sparse([NaN NaN 1]),'stable'), sparse([NaN NaN 1]));
asserts.isequal(unique(sparse(zeros(3,0))), sparse(zeros([0 1], 'double')));
asserts.isequal(unique(sparse(0,0)), sparse(zeros([0 1], 'double')));
asserts.isequal(unique(sparse(ones(2,3))), sparse(1));
%=============================================================================
