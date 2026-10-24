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
surf(peaks(20));
ax = gca();
camtarget(ax, [0 0 0]);
campos(ax, [0 -30 0]);
camup(ax, [0 0 1]);
%=============================================================================
% Panning rotates the target around the camera: the position is unchanged
% and the camera-target distance is preserved.
p0 = campos(ax);
d0 = norm(campos(ax) - camtarget(ax));
campan(ax, 20, 0);
asserts.isapprox(campos(ax), p0, 1e-9);
asserts.isapprox(norm(campos(ax) - camtarget(ax)), d0, 1e-6);
asserts.istrue(norm(camtarget(ax) - [0 0 0]) > 0);
%=============================================================================
asserts.checkerror('campan(gca(), ''a'', 1)', 'Rotation angles must be real scalars.');
%=============================================================================
close all;
%=============================================================================
