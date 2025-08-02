%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% starts excel and save file
excelApplication = actxserver('Excel.Application');
eWorkbook = excelApplication.Workbooks.Add;
excelApplication.Visible = true;
eSheets = excelApplication.ActiveWorkbook.Sheets;
eSheet1 = get(eSheets, 'Item', 1);
eSheet1.Activate
A = [pi() 2.4; 3.4444 4.5555];
eActivesheetRange = get(excelApplication.Activesheet,'Range','A1:B2');
eActivesheetRange.Value = A;
eActivesheetRange.Value

eRange = get(excelApplication.Activesheet,'Range','A1:B2');
B = eRange.Value;
B = reshape([B{:}], size(B));

destfilename =  [getenv('TEMP'), '\myfile.xls'];
if isfile(destfilename)
  rmfile(destfilename);
end

invoke(eWorkbook, 'SaveAs', destfilename)
eWorkbook.Saved = 1;

eWorkbook.Close;
excelApplication.Quit;
delete(excelApplication)
clear excelApplication
%=============================================================================
