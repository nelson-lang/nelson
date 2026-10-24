%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(reshape([],0,[]), zeros([0 0], 'double'));
asserts.isequal(reshape([],[],0), zeros([0 0], 'double'));
asserts.isequal(reshape(zeros(0,3),0,[]), zeros([0 0], 'double'));
asserts.isequal(reshape(zeros(0,3),[],0), zeros([0 0], 'double'));
asserts.isequal(reshape(zeros(3,0),[],0), zeros([0 0], 'double'));
asserts.isequal(reshape(zeros(3,0),0,[]), zeros([0 0], 'double'));
asserts.isequal(reshape(zeros(0,3),0,[],7), zeros([0 0 7], 'double'));
asserts.isequal(reshape(zeros(0,3),0,0,[]), zeros([0 0 0], 'double'));
asserts.isequal(reshape(zeros(0,3),0,7,[]), zeros([0 7 0], 'double'));
asserts.isequal(reshape(zeros(0,3),[],0,7), zeros([0 0 7], 'double'));
%=============================================================================
