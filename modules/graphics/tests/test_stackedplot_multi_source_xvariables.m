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
[T, rowTimes, temperature, ~, rain] = graphics_stackedplot_fixture();
T3 = table((11:16)', temperature + 2, rain + 0.2, ...
  'VariableNames', {'Sample', 'Temperature', 'Rain'});
s = stackedplot(T, T3, {'Temperature'}, 'XVariable', {'Time', 'Sample'});
asserts.isequal(s.XVariable, {'Time', 'Sample'});
asserts.isequal(s.Children(1).Children(end).XData, datenum(rowTimes)');
asserts.isequal(s.Children(1).Children(end - 1).XData, 11:16);
close all;
%=============================================================================
