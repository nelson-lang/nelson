%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% direction
asserts.isequal(cummax([1 3 2], 'reverse'), [3 3 2]);
asserts.isequal(cummin([4 2 5], 'reverse'), [2 2 5]);
asserts.isequal(cummax([1 3 2], 'forward'), [1 3 3]);
%=============================================================================
% nanflag: default is 'omitnan'
asserts.isequal(cummin([3 NaN 1], 'omitnan'), [3 3 1]);
asserts.isequal(cummax([1 NaN 2]), [1 1 2]);
asserts.isequal(cummax([NaN 2 1]), [NaN 2 2]);
asserts.isequal(cummax([1 NaN 2], 'includenan'), [1 NaN NaN]);
asserts.isequal(cummin([3 NaN 1], 'includenan'), [3 NaN NaN]);
%=============================================================================
% dimension still works, combined with options
M = [3 5 1; 2 4 8];
asserts.isequal(cummax(M), [3 5 1; 3 5 8]);
asserts.isequal(cummax(M, 2), [3 5 5; 2 4 8]);
asserts.isequal(cummax(M, 2, 'reverse'), [5 5 1; 8 8 8]);
asserts.isequal(cummin(M, 2, 'reverse'), [1 1 1; 2 4 8]);
%=============================================================================
% integer types preserved
asserts.isequal(cummax(int8([3 1 5 2])), int8([3 3 5 5]));
asserts.isequal(class(cummax(int8([3 1 5 2]))), 'int8');
%=============================================================================
