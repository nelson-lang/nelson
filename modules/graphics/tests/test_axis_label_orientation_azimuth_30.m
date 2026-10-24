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
% Axis labels re-derive their Rotation and alignment from the View (auto
% RotationMode semantics). Expected values below cover automatic behavior
% across view(2), view(3) and several azimuths.
%=============================================================================
f = figure('Visible', 'off');
plot3([0 1], [0 1], [0 1]);
ax = gca();
xlabel(ax, 'X');
ylabel(ax, 'Y');
zlabel(ax, 'Z');
%=============================================================================
% Azimuth sweep at elevation 30. The z label stays upright and centered.
view(ax, 30, 30);
drawnow();
assert_axis_label_orientation(ax, 'XLabel', 0, 'right', 'top');
assert_axis_label_orientation(ax, 'YLabel', 0, 'left', 'top');
assert_axis_label_orientation(ax, 'ZLabel', 90, 'center', 'bottom');
%=============================================================================
