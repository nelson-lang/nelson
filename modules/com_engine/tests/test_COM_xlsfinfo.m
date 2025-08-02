%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--EXCEL REQUIRED-->
%=============================================================================
assert_isequal(nargin('COM_xlsfinfo'), 1);
assert_isequal(nargout('COM_xlsfinfo'), -1);
%=============================================================================
[s, sheets, format] = COM_xlsfinfo([modulepath('com_engine', 'tests'), '/test_COM_xlsread.m']);
assert_isequal(s, 'Microsoft Excel Spreadsheet');
assert_isequal(sheets, {'test_COM_xlsread'});
assert_isequal(format, 'xlCurrentPlatformText');
%=============================================================================
[s, sheets, format] = COM_xlsfinfo([modulepath('com_engine', 'tests'), '/format_excel97.xls']);
assert_isequal(s, 'Microsoft Excel Spreadsheet');
assert_isequal(sheets, {'Feuil1', 'Feuil2'});
assert_isequal(format, 'xlExcel8');
%=============================================================================
[s, sheets, format] = COM_xlsfinfo([modulepath('com_engine', 'tests'), '/sample_xslx.xlsx']);
assert_isequal(s, 'Microsoft Excel Spreadsheet');
assert_isequal(sheets, {'Smile You Can Read Me !'});
assert_isequal(format, 'xlOpenXMLWorkbook');
%=============================================================================
