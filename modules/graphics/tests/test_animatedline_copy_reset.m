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
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
%=============================================================================
ax.ColorOrder = [1 0 0; 0 1 0; 0 0 1];
ax.LineStyleOrder = {'-'; '--'; ':'};
an = animatedline(ax);
an.SeriesIndex = 2;
asserts.isequal(an.SeriesIndex, 2);
asserts.isequal(an.Color, [0 1 0]);
asserts.isequal(an.LineStyle, '--');
asserts.isequal(an.Marker, 'none');
an.Marker = '+';
asserts.isequal(an.MarkerMode, 'manual');
an.SeriesIndex = 3;
asserts.isequal(an.Marker, '+');
asserts.isequal(an.Color, [0 0 1]);
an.Color = [0.2 0.3 0.4];
asserts.isequal(an.ColorMode, 'manual');
an.ColorMode = 'auto';
an.SeriesIndex = 1;
asserts.isequal(an.Color, [1 0 0]);
%=============================================================================
clearpoints(an);
addpoints(an, [0 1 2], [0 1 0], [3 4 5]);
[x, y, z] = getpoints(an);
asserts.isequal([x; y; z], [0 1 2; 0 1 0; 3 4 5]);
copied = copyobj(an, ax);
asserts.istrue(isgraphics(copied, 'animatedline'));
[cx, cy, cz] = getpoints(copied);
asserts.isequal([cx; cy; cz], [x; y; z]);
reset(copied);
[cx, cy, cz] = getpoints(copied);
asserts.istrue(isempty(cx));
asserts.istrue(isempty(cy));
asserts.istrue(isempty(cz));
asserts.isequal(copied.MaximumNumPoints, 1000000);
%=============================================================================
