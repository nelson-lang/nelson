%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Write a table to a workbook and read it back.
filename = [tempdir(), 'nelson-weather.xlsx'];
weather = table({'Mon'; 'Tue'; 'Wed'}, [18.2; 20.1; 19.4], ...
  'VariableNames', {'Day', 'Temperature'});
writetable(weather, filename, 'WriteMode', 'replacefile');
restored = readtable(filename);
disp(restored);
disp(['Workbook: ', filename]);
