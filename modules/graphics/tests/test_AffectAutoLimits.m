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
f = figure('Visible', 'off');
ax = axes('Parent', f);
h1 = line(ax, [0 1], [0 1]);
h2 = line(ax, [0 2], [0 50]);
%=============================================================================
% default: both objects drive the automatic limits
assert_isequal(char(h1.AffectAutoLimits), 'on');
assert_istrue(ax.YLim(2) >= 40);
%=============================================================================
% excluding h2 drops the tall data from the automatic limits (force a recompute)
h2.AffectAutoLimits = 'off';
ax.YLimMode = 'manual';
ax.YLimMode = 'auto';
assert_istrue(ax.YLim(2) < 10);
%=============================================================================
close(f);
%=============================================================================
