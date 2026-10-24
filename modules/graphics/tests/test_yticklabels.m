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
barh([10 20 30 41]);
yticklabels({'April', 'May', 'June', 'July'});
ax = gca();
asserts.isequal(ax.XLim, [0 45]);
asserts.isequal(ax.XTick, 0:5:45);
asserts.isequal(ax.YTickLabel, {'April'; 'May'; 'June'; 'July'});
asserts.isequal(ax.YTickLabelMode, 'manual');
asserts.isequal(yticklabels(), {'April'; 'May'; 'June'; 'July'});
asserts.isequal(yticklabels('mode'), 'manual');
yticklabels('auto');
asserts.isequal(ax.YTickLabelMode, 'auto');
yticklabels(ax, ["A"; "B"; "C"; "D"]);
asserts.isequal(ax.YTickLabel, {'A'; 'B'; 'C'; 'D'});
close(f);
%=============================================================================
close all
%=============================================================================
% an empty value clears the tick labels (yticklabels([]))
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:5);
yticklabels(ax, {'a', 'b'});
assert_isequal(numel(ax.YTickLabel), 2);
yticklabels(ax, []);
assert_istrue(isempty(ax.YTickLabel));
assert_isequal(ax.YTickLabelMode, 'manual');
close(f);
%=============================================================================
