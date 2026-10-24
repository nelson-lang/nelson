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
close all;
f = figure('Visible', 'off');
[x, y, z] = sphere(16);
s = surf(x * 2 + 5, y * 2 - 3, z * 2 + 1);
ax = gca();
campos(ax, [40 40 40]);
camtarget(ax, [0 0 0]);
%=============================================================================
% Looking at the object recenters the target on the object center and keeps
% the camera on the previous viewing direction.
dir0 = campos(ax) - camtarget(ax);
dir0 = dir0 / norm(dir0);
camlookat(s);
t = camtarget(ax);
asserts.isapprox(t, [5 -3 1], 1e-6);
dir1 = campos(ax) - camtarget(ax);
dir1 = dir1 / norm(dir1);
asserts.isapprox(dir1, dir0, 1e-6);
%=============================================================================
% camlookat on the axes uses the axes limits.
camlookat(ax);
asserts.istrue(isfinite(norm(campos(ax))));
%=============================================================================
close all;
%=============================================================================
