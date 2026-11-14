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
f = figure('Visible', 'off');
%=============================================================================
% a FontUnits change converts FontSize (points <-> pixels is the 4/3 factor)
u = uicontrol(f, 'Style', 'text', 'FontSize', 10, 'FontUnits', 'points');
u.FontUnits = 'pixels';
assert_isapprox(u.FontSize, 13.3333333333333, 1e-10);
assert_isequal(u.FontUnits, 'pixels');
u.FontUnits = 'points';
assert_isapprox(u.FontSize, 10, 1e-10);
%=============================================================================
% 72 points is one inch
u2 = uicontrol(f, 'Style', 'text', 'FontSize', 72, 'FontUnits', 'points');
u2.FontUnits = 'inches';
assert_isapprox(u2.FontSize, 1, 1e-10);
%=============================================================================
% a FontSize given after FontUnits in the same set is already in the new units
u3 = uicontrol(f, 'Style', 'text');
set(u3, 'FontUnits', 'pixels', 'FontSize', 20);
assert_isequal(u3.FontSize, 20);
assert_isequal(u3.FontUnits, 'pixels');
%=============================================================================
close(f);
%=============================================================================
