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
campos(ax, [0 0 30]);
camup(ax, [0 1 0]);
%=============================================================================
% Roll rotates the up vector about the viewing axis (here the z axis).
camroll(ax, 90);
u = camup(ax);
asserts.isapprox(norm(u), 1, 1e-6);
% up [0 1 0] rolled 90 deg about +z view axis -> about [-1 0 0] (or [1 0 0]).
asserts.isapprox(abs(u(1)), 1, 1e-4);
asserts.istrue(abs(u(3)) < 1e-6);
%=============================================================================
asserts.checkerror('camroll(gca(), ''x'')', 'Roll angle must be a real scalar.');
%=============================================================================
close all;
%=============================================================================
