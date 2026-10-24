%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% omitnan / includenan
asserts.isequal(movmean([1 NaN 3], 3, 'omitnan'), [1 2 3]);
asserts.isequal(movmean([1 NaN 3], 3), [NaN NaN NaN]);
asserts.isequal(movmedian([1 NaN 3], 3, 'omitnan'), [1 2 3]);
asserts.isequal(movsum([1 NaN 3], 3, 'omitnan'), [1 4 3]);
asserts.isequal(movsum([NaN NaN NaN], 3, 'omitnan'), [0 0 0]);
asserts.isequal(movmean([NaN NaN], 2, 'omitnan'), [NaN NaN]);
asserts.isequal(movstd([1 NaN 3], 3, 'omitnan'), [0 sqrt(2) 0]);
asserts.isequal(movvar([1 NaN 3], 3, 'omitnan'), [0 2 0]);
asserts.isequal(movprod([1 NaN 3], 3, 'omitnan'), [1 3 3]);
% movmax / movmin default is 'omitnan'
asserts.isequal(movmax([1 NaN 3], 3), [1 3 3]);
asserts.isequal(movmax([1 NaN 3], 3, 'includenan'), [NaN NaN NaN]);
asserts.isequal(movmin([3 NaN 1], 3, 'includenan'), [NaN NaN NaN]);
%=============================================================================
% window given as [kb kf]
asserts.isequal(movmean(1:5, [1 0]), [1 1.5 2.5 3.5 4.5]);
asserts.isequal(movsum(1:5, [0 2]), [6 9 12 9 5]);
%=============================================================================
% even scalar window is centered about current and previous elements
asserts.isequal(movmean(1:5, 2), [1 1.5 2.5 3.5 4.5]);
%=============================================================================
% Endpoints
asserts.isequal(movsum(1:5, 3, 'Endpoints', 'discard'), [6 9 12]);
asserts.isequal(movmean(1:5, 3, 'Endpoints', 'discard'), [2 3 4]);
asserts.isequal(movsum(1:5, [2 1], 'Endpoints', 'discard'), [10 14]);
asserts.isequal(movsum(1:5, 3, 'Endpoints', 'fill'), [NaN 6 9 12 NaN]);
asserts.isequal(movmean(1:5, 3, 'Endpoints', 'fill'), [NaN 2 3 4 NaN]);
% 'fill' pads the window with NaN: 'omitnan' ignores them (as 'shrink'),
% and movmax/movmin ignore them by default
asserts.isequal(movmean([2 4 6], [1 0], 'omitnan', 'Endpoints', 'fill'), [2 3 5]);
asserts.isequal(movmax(1:5, 3, 'Endpoints', 'fill'), [2 3 4 5 5]);
%=============================================================================
% dimension argument still works
asserts.isequal(movsum([1 2 3; 4 5 6], 3, 2), [3 6 5; 9 15 11]);
%=============================================================================
