%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(mode(sparse([1 2 2 3])), sparse(2));
asserts.isequal(mode('aabc'), char([97]));
asserts.isequal(mode([true true false]), true);
asserts.isequal(mode([1 1;2 2],'all'), 1);
asserts.isequal(mode(ones(2,3),[1 2]), 1);
asserts.isequal(mode(ones(2,3),4), reshape([1 1 1 1 1 1], [2 3]));
asserts.isequal(mode(true(0,0)), false);
asserts.checkerror('mode(ones(2,3),[])', _('Dimension argument must be a positive integer scalar or vector of positive integers.'));
asserts.checkerror('mode(ones(2,3),[1 1])', _('Dimension argument must be a positive integer scalar or vector of positive integers.'));
asserts.checkerror('mode(ones(2,3),true)', _('Dimension argument must be a positive integer scalar or vector of positive integers.'));
asserts.isequal(mode(ones(2,3),'all'), 1);
asserts.isequal(mode(ones(2,3),"all"), 1);
asserts.isequal(mode(ones(2,3),[1 2 3]), 1);
asserts.isequal(mode(ones(2,3),[2 1]), 1);
asserts.isequal(mode(ones(2,3),[1 3]), [1 1 1]);
asserts.isequal(mode(sparse(ones(2,3)),2), sparse(reshape([1 1], [2 1])));
asserts.isequal(mode(sparse(zeros(0,3))), sparse([NaN NaN NaN]));
asserts.isequal(mode(sparse([])), sparse(NaN));
asserts.isequal(mode(true(2,2)), logical([1 1]));
asserts.isequal(mode(false(2,2)), logical([0 0]));
asserts.isequal(mode(zeros(0,3),'all'), NaN);
asserts.isequal(mode(zeros(0,0),'all'), NaN);
asserts.isequal(mode(true(0,0),'all'), false);
%=============================================================================
