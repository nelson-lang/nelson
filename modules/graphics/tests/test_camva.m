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
%=============================================================================
v = camva();
asserts.istrue(isscalar(v));
asserts.isequal(camva(ax), get(ax, 'CameraViewAngle'));
%=============================================================================
% Setting the view angle switches the mode to manual and keeps the value.
camva(ax, 25);
asserts.isequal(camva(ax), 25);
asserts.isequal(char(camva(ax, 'mode')), 'manual');
%=============================================================================
camva(ax, 'auto');
asserts.isequal(char(camva(ax, 'mode')), 'auto');
%=============================================================================
asserts.checkerror('camva(gca(), -1)', 'Camera view angle must be a real scalar in the range (0, 180).');
asserts.checkerror('camva(gca(), 200)', 'Camera view angle must be a real scalar in the range (0, 180).');
%=============================================================================
close all;
%=============================================================================
