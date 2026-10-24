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
close all
%=============================================================================
f = figure('Visible', 'off');
bar([10 20 30 41]);
xticks(1:4);
xticklabels({'April', 'May', 'June', 'July'});
ax = gca();
assert_isequal(ax.YLim, [0 45]);
assert_isequal(ax.YTick, 0:5:45);
assert_isequal(ax.XTickLabel, {'April'; 'May'; 'June'; 'July'});
assert_isequal(ax.XTickLabelMode, 'manual');
assert_isequal(xticklabels(), {'April'; 'May'; 'June'; 'July'});
assert_isequal(xticklabels('mode'), 'manual');
xticklabels('auto');
assert_isequal(ax.XTickLabelMode, 'auto');
xticklabels(ax, ["A"; "B"; "C"; "D"]);
assert_isequal(ax.XTickLabel, {'A'; 'B'; 'C'; 'D'});
close(f);
%=============================================================================
close all
%=============================================================================
% an empty value clears the tick labels (xticklabels([]))
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:5);
xticklabels(ax, {'a', 'b'});
assert_isequal(numel(ax.XTickLabel), 2);
xticklabels(ax, []);
assert_istrue(isempty(ax.XTickLabel));
assert_isequal(ax.XTickLabelMode, 'manual');
close(f);
%=============================================================================
