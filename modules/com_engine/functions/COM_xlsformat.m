%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [name, description, value] = COM_xlsformat(formatAsInt)
  searchValue = double(formatAsInt);
  
  formats = { ...
  {'xlAddIn', 18, 'Microsoft Excel 97-2003 Add-In'}, ...
  {'xlAddIn8', 18, 'Microsoft Excel 97-2003 Add-In'}, ...
  {'xlCSV', 6, 'CSV'}, ...
  {'xlCSVMac', 22, 'Macintosh CSV'}, ...
  {'xlCSVMSDOS', 24, 'MSDOS CSV'}, ...
  {'xlCSVWindows', 23, 'Windows CSV'}, ...
  {'xlCurrentPlatformText', -4158, 'Current Platform Text'}, ...
  {'xlDBF2', 7, 'DBF2'}, ...
  {'xlDBF3', 8, 'DBF3'}, ...
  {'xlDBF4', 11, 'DBF4'}, ...
  {'xlDIF', 9, 'DIF'}, ...
  {'xlExcel12', 50, 'Excel12'}, ...
  {'xlExcel2', 16, 'Excel2'}, ...
  {'xlExcel2FarEast', 27, 'Excel2 FarEast'}, ...
  {'xlExcel3', 29, 'Excel3'}, ...
  {'xlExcel4', 33, 'Excel4'}, ...
  {'xlExcel4Workbook', 35, 'Excel4 Workbook'}, ...
  {'xlExcel5', 39, 'Excel5'}, ...
  {'xlExcel7', 39, 'Excel7'}, ...
  {'xlExcel8', 56, 'Excel8'}, ...
  {'xlExcel9795', 43, 'Excel9795'}, ...
  {'xlHtml', 44, 'HTML format'}, ...
  {'xlIntlAddIn', 26, 'International Add-In'}, ...
  {'xlIntlMacro', 25, 'International Macro'}, ...
  {'xlOpenDocumentSpreadsheet', 60, 'OpenDocument Spreadsheet'}, ...
  {'xlOpenXMLAddIn', 55, 'Open XML Add-In'}, ...
  {'xlOpenXMLStrictWorkbook', 61, 'Strict Open XML file'}, ...
  {'xlOpenXMLTemplate', 54, 'Open XML Template'}, ...
  {'xlOpenXMLTemplateMacroEnabled', 53, 'Open XML Template Macro Enabled'}, ...
  {'xlOpenXMLWorkbookMacroEnabled', 52, 'Open XML Workbook Macro Enabled'}, ...
  {'xlSYLK', 2, 'SYLK'}, ...
  {'xlTemplate', 17, 'Template'}, ...
  {'xlTemplate8', 17, 'Template 8'}, ...
  {'xlTextMac', 19, 'Macintosh Text'}, ...
  {'xlTextMSDOS', 21, 'MSDOS Text'}, ...
  {'xlTextPrinter', 36, 'Printer Text'}, ...
  {'xlTextWindows', 20, 'Windows Text'}, ...
  {'xlUnicodeText', 42, 'Unicode Text'}, ...
  {'xlWebArchive', 45, 'Web Archive'}, ...
  {'xlWJ2WD1', 14, 'WJ2WD1'}, ...
  {'xlWJ3', 40, 'WJ3'}, ...
  {'xlWJ3FJ3', 41, 'WJ3FJ3'}, ...
  {'xlWK1', 5, 'WK1'}, ...
  {'xlWK1ALL', 31, 'WK1ALL'}, ...
  {'xlWK1FMT', 30, 'WK1FMT'}, ...
  {'xlWK3', 15, 'WK3'}, ...
  {'xlWK3FM3', 32, 'WK3FM3'}, ...
  {'xlWK4', 38, 'WK4'}, ...
  {'xlWKS', 4, 'Worksheet'}, ...
  {'xlOpenXMLWorkbook', 51, 'Open XML Workbook'}, ...
  {'xlWorkbookDefault', 51, 'Workbook default'}, ...
  {'xlWorkbookNormal', -4143, 'Workbook normal'}, ...
  {'xlWorks2FarEast', 28, 'Works2 FarEast'}, ...
  {'xlWQ1', 34, 'WQ1'}, ...
  {'xlXMLSpreadsheet', 46, 'XML Spreadsheet'}};
  
  found = {};
  for c = formats(:)'
    
    if c{1}{2} == searchValue
      found = c{1};
      break;
    end
  end
  if isempty(found)
    error(_('format not found.'));
  end
  
  name = found{1};
  value = found{2};
  description = found{3};
end
%=============================================================================
