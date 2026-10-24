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
% A user HorizontalAlignment is likewise frozen while Rotation stays auto: the
% pinned 'left' survives a view whose auto alignment would be 'right'.
f = figure('Visible', 'off');
plot([0 1], [0 1]);
ax = gca();
hx = xlabel(ax, 'X');
set(hx, 'HorizontalAlignment', 'left');
view(ax, 60, 30);
drawnow();
asserts.isequal(hx.Rotation, 0);
asserts.isequal(hx.HorizontalAlignment, 'left');
close(f);
%=============================================================================
