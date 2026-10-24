%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% vectors are processed element by element
asserts.isequal(rmmissing([1 NaN 3]), [1 3]);
asserts.isequal(rmmissing([1; NaN; 3]), [1; 3]);
asserts.istrue(isempty(rmmissing([NaN NaN])));
asserts.isequal(rmmissing(5), 5);
%=============================================================================
% matrices: rows containing missing values are removed
asserts.isequal(rmmissing([1 2; NaN 4; 5 6]), [1 2; 5 6]);
% explicit dimension
asserts.isequal(rmmissing([1 2 NaN; 4 5 6], 2), [1 2; 4 5]);
%=============================================================================
