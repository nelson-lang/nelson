%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
asserts.checkerror('lightangle(ax, [0 1], 20)', getString(message('Nelson:lightangle:azimuthAndElevationExpected')));
asserts.checkerror('camlight(ax, ''bad'')', getString(message('Nelson:graphics:invalidInputArguments')));
asserts.checkerror('camlight(ax, [0 1], 20)', getString(message('Nelson:graphics:invalidInputArguments')));
asserts.checkerror('light(ax, ''Style'', ''spot'')', graphicsPropertyErrorMessage('Style', 'Nelson:graphics:illegalSelection'));
asserts.checkerror('light(ax, ''Color'', [1 2])', graphicsPropertyErrorMessage('Color', 'Nelson:color:invalidColorSpec'));
close all
%=============================================================================
