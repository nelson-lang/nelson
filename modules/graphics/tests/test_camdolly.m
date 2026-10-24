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
% Dollying moves the camera position and target by the same amount: the
% distance between them is preserved.
p0 = campos(ax);
t0 = camtarget(ax);
camdolly(ax, 0.2, 0.1, 0);
p1 = campos(ax);
t1 = camtarget(ax);
asserts.isapprox(norm(p1 - t1), norm(p0 - t0), 1e-6);
asserts.isapprox(p1 - p0, t1 - t0, 1e-9);
asserts.istrue(norm(p1 - p0) > 0);
%=============================================================================
% 'data' coordinate system with explicit units moves along world axes.
campos(ax, [0 0 30]);
camtarget(ax, [0 0 0]);
camdolly(ax, 1, 2, 3, 'data', 'data');
asserts.isapprox(camtarget(ax), [1 2 3], 1e-9);
%=============================================================================
asserts.checkerror('camdolly(gca(), ''a'', 0, 0)', 'dx, dy and dz must be real scalars.');
%=============================================================================
close all;
%=============================================================================
