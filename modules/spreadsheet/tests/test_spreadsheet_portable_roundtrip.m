%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
csv_filename = [tempname(), '.csv'];
xlsx_filename = [tempname(), '.xlsx'];
json_filename = [tempname(), '.json'];
%=============================================================================
M = [1 2; 3 4];
writematrix(M, csv_filename);
asserts.isequal(readmatrix(csv_filename), M);
%=============================================================================
writematrix(M, xlsx_filename, 'FileType', 'spreadsheet', ...
  'Sheet', 'Data', 'Range', 'B2', 'WriteMode', 'replacefile');
R = readmatrix(xlsx_filename, 'FileType', 'spreadsheet', ...
  'Sheet', 'Data', 'Range', 'B2:C3');
asserts.isequal(R, M);
%=============================================================================
Name = {'A'; 'B'};
Value = [10; 20];
T = table(Name, Value);
writetable(T, json_filename, 'FileType', 'json', 'PrettyPrint', false);
RT = readtable(json_filename, 'FileType', 'json');
asserts.istrue(istable(RT));
asserts.isequal(RT.Properties.VariableNames, {'Name', 'Value'});
asserts.isequal(RT.Name, ["A"; "B"]);
asserts.isequal(RT.Value, Value);
%=============================================================================
rmfile(csv_filename);
rmfile(xlsx_filename);
rmfile(json_filename);
%=============================================================================
