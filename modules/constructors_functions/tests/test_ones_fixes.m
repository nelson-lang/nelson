%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(ones([]), zeros([0 0], 'double'));
asserts.isequal(ones(2,[]), zeros([2 0], 'double'));
asserts.isequal(ones([],'int8'), zeros([0 0], 'int8'));
asserts.isequal(ones(true), 1);
asserts.isequal(ones(1,2,"single"), single([1 1]));
asserts.isequal(ones(2,3,'Like',single(1)), single(reshape([1 1 1 1 1 1], [2 3])));
asserts.isequal(ones([],3), zeros([0 3], 'double'));
asserts.isequal(ones(3,[]), zeros([3 0], 'double'));
asserts.isequal(ones(zeros(1,0)), zeros([0 0], 'double'));
asserts.isequal(ones(zeros(0,1)), zeros([0 0], 'double'));
%=============================================================================
