%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
[~, ~, temperature, pressure, rain] = graphics_stackedplot_fixture();
%=============================================================================
s = stackedplot([temperature, pressure, rain]);
asserts.isequal(s.DisplayVariables, {});
asserts.isequal(s.DisplayLabels, {'Column 1'; 'Column 2'; 'Column 3'});
asserts.isequal(s.Children(1).Children(end).XData, 1:6);
asserts.isequal(s.Children(2).Children(end).YData, pressure');
close all;
%=============================================================================
s = stackedplot((10:15)', [temperature, pressure, rain]);
asserts.isequal(s.DisplayLabels, {'Column 1'; 'Column 2'; 'Column 3'});
asserts.isequal(s.Children(1).Children(end).XData, 10:15);
close all;
%=============================================================================
