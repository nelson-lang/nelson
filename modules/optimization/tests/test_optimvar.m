%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = optimvar('x', 2, 1, 'LowerBound', 0, 'UpperBound', 3);
asserts.isequal(x.Name, 'x');
asserts.isequal(x.Size, [2 1]);
asserts.isequal(x.LowerBound, 0);
asserts.isequal(x.UpperBound, 3);
%=============================================================================
