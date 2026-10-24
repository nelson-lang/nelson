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
camva(ax, 20);
%=============================================================================
% zf > 1 zooms in: the tangent of the half view angle is divided by the factor.
camzoom(ax, 2);
asserts.isapprox(camva(ax), 2 * atand(tand(10) / 2), 1e-9);
asserts.isapprox(camva(ax), 10.076738, 1e-6);
%=============================================================================
% zf < 1 zooms out.
camzoom(ax, 0.5);
asserts.isapprox(camva(ax), 20, 1e-9);
%=============================================================================
% Wide angles show the difference with a plain division of the angle.
camva(ax, 120);
camzoom(ax, 2);
asserts.isapprox(camva(ax), 81.786789, 1e-6);
%=============================================================================
asserts.checkerror('camzoom(gca(), -1)', 'Zoom factor must be a positive real scalar.');
%=============================================================================
close all;
%=============================================================================
