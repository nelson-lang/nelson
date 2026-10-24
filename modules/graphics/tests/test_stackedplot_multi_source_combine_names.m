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
T2 = table(rowTimes, temperature + 1, rain + 0.1, ...
  'VariableNames', {'Time', 'Temperature', 'Rain'});
s = stackedplot({T, T2}, {'Temperature'}, 'XVariable', 'Time', ...
  'CombineMatchingNames', false);
asserts.isequal(length(s.Children), 2);
asserts.isequal(s.DisplayLabels, {'Temperature (1)'; 'Temperature (2)'});
close all;
%=============================================================================
