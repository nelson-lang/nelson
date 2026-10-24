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
% Orbiting rotates the camera around the target: the distance is preserved.
d0 = norm(campos(ax) - camtarget(ax));
camorbit(ax, 90, 0);
d1 = norm(campos(ax) - camtarget(ax));
asserts.isapprox(d0, d1, 1e-6);
% A 90 degree horizontal orbit about +z moves [0 -30 0] to about [30 0 0].
p = campos(ax);
asserts.isapprox(p(1), 30, 1e-4);
asserts.istrue(abs(p(2)) < 1e-4);
%=============================================================================
camorbit(ax, 0, 30);
asserts.isapprox(norm(campos(ax) - camtarget(ax)), d0, 1e-6);
%=============================================================================
asserts.checkerror('camorbit(gca(), ''a'', ''b'')', 'Rotation angles must be real scalars.');
%=============================================================================
close all;
%=============================================================================
