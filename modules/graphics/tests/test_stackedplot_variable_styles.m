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
[T, rowTimes] = graphics_stackedplot_fixture();
%=============================================================================
s = stackedplot(T, [2 4], 'XVariable', 'Time');
asserts.isequal(s.DisplayVariables, {'Temperature', 'Rain'});
asserts.isequal(s.DisplayLabels, {'Temperature'; 'Rain'});
asserts.isequal(s.XVariable, 'Time');
asserts.isequal(s.XLabel, 'Time');
asserts.isequal(s.Children(1).Children(end).XData, datenum(rowTimes)');
close all;
%=============================================================================
