%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(eye([]), zeros([0 0], 'double'));
asserts.isequal(eye(2,[]), zeros([2 0], 'double'));
asserts.isequal(eye([],2), zeros([0 2], 'double'));
asserts.isequal(eye(zeros(1,0)), zeros([0 0], 'double'));
asserts.isequal(eye(zeros(0,1)), zeros([0 0], 'double'));
%=============================================================================
