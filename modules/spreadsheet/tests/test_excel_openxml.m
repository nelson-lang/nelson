%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
xlsx_filename = [tempname(), '_nelson_excel_openxml_matrix.xlsx'];
writematrix([1 2; 3 4], xlsx_filename, 'FileType', 'spreadsheet', ...
  'Sheet', 'Data', 'Range', 'B2', 'WriteMode', 'replacefile');
R = readmatrix(xlsx_filename, 'FileType', 'spreadsheet', ...
  'Sheet', 'Data', 'Range', 'B2:C3');
asserts.isequal(R, [1 2; 3 4]);
%=============================================================================
R = readmatrix(xlsx_filename, 'Sheet', 'Data', 'Range', 'B2:C3', ...
  'OutputType', 'cell');
asserts.isequal(R, {1, 2; 3, 4});
%=============================================================================
writematrix([5 6], xlsx_filename, 'Sheet', 'Data', 'Range', 'B4', ...
  'WriteMode', 'inplace');
R = readmatrix(xlsx_filename, 'Sheet', 'Data', 'Range', 'B2:C4');
asserts.isequal(R, [1 2; 3 4; 5 6]);
%=============================================================================
cell_filename = [tempname(), '_nelson_excel_openxml_cell.xlsx'];
C = {'Name', 'Value', 'Flag'; 'A', 3.5, true; 'B', [], false};
writecell(C, cell_filename, 'Sheet', 2, 'Range', 'A1', 'WriteMode', 'replacefile');
R = readcell(cell_filename, 'Sheet', 2, 'Range', 'A1:C3');
asserts.isequal(R{1, 1}, 'Name');
asserts.isequal(R{2, 1}, 'A');
asserts.isequal(R{2, 2}, 3.5);
asserts.isequal(R{2, 3}, 1);
asserts.isequal(R{3, 3}, 0);
%=============================================================================
table_filename = [tempname(), '_nelson_excel_openxml_table.xlsx'];
Name = {'A'; 'B'};
Value = [10; 20];
Flag = logical([1; 0]);
T = table(Name, Value, Flag);
writetable(T, table_filename, 'Sheet', 'TableData', 'WriteMode', 'replacefile');
RT = readtable(table_filename, 'Sheet', 'TableData');
asserts.istrue(istable(RT));
asserts.isequal(RT.Properties.VariableNames, {'Name', 'Value', 'Flag'});
asserts.isequal(RT.Name{1}, 'A');
asserts.isequal(RT.Value(2), 20);
asserts.isequal(RT.Flag(1), 1);
%=============================================================================
display_filename = [tempname(), '_nelson_excel_openxml_display_table.xlsx'];
C = {'Invoice ID', 'Customer type', 'Unit price', 'Tax 5%', 'Time'; ...
  '371-85-5789', 'Normal', 87.98, 13.197, '10:40:00'; ...
  '370-41-7321', 'Member', 56.69, 25.5105, '17:24:00'};
writecell(C, display_filename, 'WriteMode', 'replacefile');
DT = readtable(display_filename);
asserts.istrue(istable(DT));
asserts.isequal(DT.Properties.VariableNames, ...
  {'InvoiceID', 'CustomerType', 'UnitPrice', 'Tax5_', 'Time'});
asserts.isequal(DT.UnitPrice, [87.98; 56.69]);
asserts.isequal(DT.Time{1}, '10:40:00');
displayText = evalc('disp(DT)');
asserts.istrue(contains(displayText, 'InvoiceID'));
asserts.istrue(contains(displayText, 'UnitPrice'));
asserts.isfalse(contains(displayText, 'table with properties'));
implicitDisplayText = evalc('DT');
asserts.istrue(contains(implicitDisplayText, 'InvoiceID'));
asserts.istrue(contains(implicitDisplayText, 'UnitPrice'));
asserts.isfalse(contains(implicitDisplayText, 'table with properties'));
health_filename = [modulepath('spreadsheet'), '/tests/Health_and_beauty.xlsx'];
HT = readtable(health_filename);
asserts.isequal(size(HT), [152 17]);
asserts.isequal(HT.Properties.VariableNames(1:5), ...
  {'InvoiceID', 'Branch', 'City', 'CustomerType', 'Gender'});
healthDisplayText = evalc('HT');
asserts.istrue(contains(healthDisplayText, 'InvoiceID'));
asserts.istrue(contains(healthDisplayText, 'UnitPrice'));
asserts.isfalse(contains(healthDisplayText, 'table with properties'));
%=============================================================================
TT = timetable(seconds([1; 2]), [10; 20], 'VariableNames', {'Value'});
tt_filename = [tempname(), '_nelson_excel_openxml_timetable.xlsx'];
writetimetable(TT, tt_filename, 'Sheet', 'TT', 'WriteMode', 'replacefile');
RTT = readtimetable(tt_filename, 'Sheet', 'TT');
asserts.istrue(istimetable(RTT));
asserts.isequal(RTT.Properties.RowTimes, seconds([1; 2]));
asserts.isequal(RTT.Value(2), 20);
%=============================================================================
xls_filename = [tempname(), '_nelson_excel_openxml_xls_api.xlsx'];
[status, message] = xlswrite(xls_filename, [7 8; 9 10], 'LegacySheet', 'C3');
asserts.istrue(status);
asserts.isequal(message, '');
[num, txt, raw] = xlsread(xls_filename, 'LegacySheet', 'C3:D4');
asserts.isequal(num, [7 8; 9 10]);
asserts.isequal(txt, cell(2, 2));
asserts.isequal(raw, {7, 8; 9, 10});
[status, sheets, format] = xlsfinfo(xls_filename);
asserts.isequal(status, 'Open XML Spreadsheet');
asserts.istrue(any(strcmp(sheets, 'LegacySheet')));
asserts.isequal(format, 'Open XML Spreadsheet');
%=============================================================================
xls_unsupported = [modulepath('spreadsheet'), '/tests/file_example_XLS_100.xls'];
didError = false;
try
  xlsread(xls_unsupported);
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
didError = false;
try
  xlsfinfo(xls_unsupported);
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
[status, message] = xlswrite(xls_unsupported, 1);
asserts.isfalse(status);
asserts.istrue(contains(message, '.xlsx'));
%=============================================================================
didError = false;
try
  readmatrix(xls_unsupported, 'FileType', 'spreadsheet');
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
