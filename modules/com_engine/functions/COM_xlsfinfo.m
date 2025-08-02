%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = COM_xlsfinfo(filename)
  status = 'Microsoft Excel Spreadsheet';
  sheets = {};
  format = [];
  if ~isfile(filename)
    error(_('Valid filename expected.'));
  end
  
  try
    excelApplication = actxserver('Excel.Application');
  catch
    error(_('Excel application expected.'));
  end
  excelApplication.DisplayAlerts = false;
  workBooks = excelApplication.workbooks;
  workBook = invoke( workBooks, 'Open', filename);
  for i = 1:500
    format = workBook.FileFormat;
  end
  
  if isempty(format)
    format = workBook.FileFormat;
  end
  
  workSheets = workBook.Worksheets;
  nbSheets = double(workSheets.Count);
  sheets = cell(1, nbSheets);
  for idx = 1:nbSheets
    sheet = get(workSheets, 'item', idx);
    sheets{idx} = sheet.Name;
  end
  
  excelApplication.Quit;
  delete(excelApplication);
  clear excelApplication
  
  format = COM_xlsformat(format);
  varargout{1} = status;
  varargout{2} = sheets;
  varargout{3} = format;
  
end
%=============================================================================
