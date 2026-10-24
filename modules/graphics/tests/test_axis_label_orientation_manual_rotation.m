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
% A user Rotation pins that attribute (mode manual) while alignment stays auto.
f = figure('Visible', 'off');
plot([0 1], [0 1]);
ax = gca();
h = ylabel(ax, 'Y');
asserts.isequal(h.Rotation, 90);
asserts.isequal(h.HorizontalAlignment, 'center');
set(h, 'Rotation', 45);
view(ax, 3);
drawnow();
asserts.isequal(h.Rotation, 45);
asserts.isequal(h.HorizontalAlignment, 'right');
close(f);
%=============================================================================
