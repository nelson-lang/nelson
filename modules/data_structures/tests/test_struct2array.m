%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(struct2array(struct('a', 1, 'b', 2, 'c', 3)), [1 2 3]);
asserts.isequal(struct2array(struct('a', 1, 'b', [2 3], 'c', 4)), [1 2 3 4]);
asserts.isequal(struct2array(struct('x', [1; 2], 'y', [3; 4])), [1 3; 2 4]);
asserts.isequal(struct2array(struct('only', 42)), 42);
%=============================================================================
