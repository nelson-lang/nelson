%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(size(zeros(2,3,4),[1;2]), [2 3]);
asserts.isequal(size(zeros(2,3,4),[]), zeros([1 0], 'double'));
asserts.isequal(size(zeros(2,3,4),1,2), [2 3]);
asserts.isequal(size(zeros(2,3,4),1,3), [2 4]);
asserts.isequal(size(zeros(2,3,4),3,1), [4 2]);
asserts.isequal(size(zeros(2,3,4),1,2,3), [2 3 4]);
asserts.isequal(size(zeros(2,3,4),1,2,3,4), [2 3 4 1]);
asserts.checkerror('size(zeros(2,3,4),[1 0])', _('Dimension argument must be a positive integer scalar within indexing range.'));
asserts.isequal(size(zeros(2,3,4),true,true), [2 2]);
asserts.isequal(zeros([]), zeros([0 0], 'double'));
asserts.isequal(zeros([],3), zeros([0 3], 'double'));
asserts.isequal(zeros(3,[]), zeros([3 0], 'double'));
asserts.isequal(zeros(true), 0);
asserts.isequal(zeros(false), zeros([0 0], 'double'));
asserts.isequal(zeros(zeros(1,0)), zeros([0 0], 'double'));
asserts.isequal(zeros(zeros(0,1)), zeros([0 0], 'double'));
asserts.isequal(zeros(1,[]), zeros([1 0], 'double'));
asserts.isequal(zeros(2,3,[]), zeros([2 3 0], 'double'));
asserts.isequal(zeros(2,"int8"), reshape([int8(0) int8(0) int8(0) int8(0)], [2 2]));
asserts.isequal(zeros([],'like',int8(1)), zeros([0 0], 'int8'));
%=============================================================================
