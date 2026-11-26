%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% MarkerFaceAlpha and MarkerEdgeAlpha accept 'flat' (case-insensitive) and read
% it back as 'flat'; a scalar in [0, 1] replaces it.
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
s = scatter(ax, 1:3, 1:3, [], [1 2 3]);
assert_isequal(s.MarkerFaceAlpha, 1);
s.MarkerFaceAlpha = 'flat';
assert_isequal(s.MarkerFaceAlpha, 'flat');
s.MarkerEdgeAlpha = 'Flat';
assert_isequal(s.MarkerEdgeAlpha, 'flat');
s.MarkerFaceAlpha = 0.25;
assert_isequal(s.MarkerFaceAlpha, 0.25);
b = bubblechart(ax, 1:3, 1:3, 1:3);
b.MarkerFaceAlpha = 'flat';
assert_isequal(b.MarkerFaceAlpha, 'flat');
delete(f);
%=============================================================================
