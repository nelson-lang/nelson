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
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = light(ax);
asserts.istrue(isgraphics(h, 'light'));
asserts.isequal(h.Type, 'light');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.Color, [1 1 1]);
asserts.isequal(h.Position, [1 0 1]);
asserts.isequal(h.Style, 'infinite');
asserts.isequal(char(h.Visible), 'on');
asserts.istrue(isempty(h.Children));
%=============================================================================
h = light(ax, 'Color', [0.2 0.3 0.4], 'Position', [3 2 1], 'Style', 'local');
asserts.isequal(h.Color, [0.2 0.3 0.4]);
asserts.isequal(h.Position, [3 2 1]);
asserts.isequal(h.Style, 'local');
close all
%=============================================================================
