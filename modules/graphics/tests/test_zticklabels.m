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
plot3(1:10, 1:10, (1:10) .^ 2);
zticks([0 50 100]);
zticklabels({'low', 'mid', 'high'});
ax = gca();
assert_isequal(ax.ZTickLabel, {'low'; 'mid'; 'high'});
assert_isequal(ax.ZTickLabelMode, 'manual');
assert_isequal(zticklabels(), {'low'; 'mid'; 'high'});
assert_isequal(zticklabels('mode'), 'manual');
zticklabels('auto');
assert_isequal(ax.ZTickLabelMode, 'auto');
zticklabels(ax, ["A"; "B"; "C"]);
assert_isequal(ax.ZTickLabel, {'A'; 'B'; 'C'});
close(f);
%=============================================================================
close all
%=============================================================================
% an empty value clears the tick labels (zticklabels([]))
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:5);
zticklabels(ax, {'a', 'b'});
assert_isequal(numel(ax.ZTickLabel), 2);
zticklabels(ax, []);
assert_istrue(isempty(ax.ZTickLabel));
assert_isequal(ax.ZTickLabelMode, 'manual');
close(f);
%=============================================================================
