%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(impzlength([1 2 3]), 3);
asserts.isequal(impzlength(1, [1 -0.5]), 14);
asserts.isequal(impzlength(1, [1 -0.5], 1e-6), 19);
asserts.isequal(impzlength(1, [1 -0.5], 1e-3), 9);
%=============================================================================
