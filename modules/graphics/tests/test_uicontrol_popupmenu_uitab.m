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
% A popupmenu in a uitab next to axes keeps its items and value across updates.
f = figure('Visible', 'off');
g = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
uitab(g, 'Title', 'First');
t = uitab(g, 'Title', 'Second');
g.SelectedTab = t;
ax = axes('Parent', t, 'Position', [0.05 0.1 0.4 0.8]);
surf(ax, peaks(20), 'EdgeColor', 'none');
heat = axes('Parent', t, 'Position', [0.55 0.1 0.4 0.6]);
C = magic(6);
C(C > 30) = NaN;
img = imagesc(heat, C);
img.AlphaData = ~isnan(C);
colorbar(heat);
items = {'Fuite'; 'VTE'; 'VTI'; ['Fr', char(233), 'quence']; 'PIP'};
p = uicontrol(t, 'Style', 'popupmenu', 'String', items, 'Value', 1, ...
  'Units', 'normalized', 'Position', [0.66 0.9 0.2 0.05], 'Callback', @(s, e) 1);
f.Visible = 'on';
drawnow();
asserts.isequal(p.Value, 1);
asserts.isequal(p.String, items);
%=============================================================================
% updates of other objects of the tab leave the list as it is
img.CData = magic(6);
f.Position = f.Position + [0 0 10 10];
drawnow();
asserts.isequal(p.Value, 1);
asserts.isequal(p.String, items);
%=============================================================================
p.Value = 4;
drawnow();
asserts.isequal(p.Value, 4);
asserts.isequal(p.String{p.Value}, ['Fr', char(233), 'quence']);
%=============================================================================
% new items keep the value
p.String = {'a'; 'b'; 'c'; 'd'; 'e'};
drawnow();
asserts.isequal(p.Value, 4);
asserts.isequal(p.String, {'a'; 'b'; 'c'; 'd'; 'e'});
close(f);
%=============================================================================
