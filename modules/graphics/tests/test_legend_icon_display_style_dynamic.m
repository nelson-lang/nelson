%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Turning IconDisplayStyle off on a plotted object after the legend exists
% removes its entry (the reference parity).
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
a = plot(ax, 1:3, 'DisplayName', 'A');
b = plot(ax, 3:-1:1, 'DisplayName', 'B');
c = plot(ax, [2 2 2], 'DisplayName', 'C');
lg = legend(ax);
assert_isequal(lg.String(:)', {'A', 'B', 'C'});
b.Annotation.LegendInformation.IconDisplayStyle = 'off';
drawnow();
assert_isequal(lg.String(:)', {'A', 'C'});
a.Annotation.LegendInformation.IconDisplayStyle = 'off';
drawnow();
assert_isequal(lg.String(:)', {'C'});
close(f);
%=============================================================================
