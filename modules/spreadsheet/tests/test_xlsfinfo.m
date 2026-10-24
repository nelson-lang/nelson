%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '_nelson_xlsfinfo_direct.xlsx'];
[status, message] = xlswrite(filename, [1 2], 'Run1', 'A1');
asserts.istrue(status, message);
[status, sheets, format] = xlsfinfo(filename);
asserts.isequal(status, 'Open XML Spreadsheet');
asserts.istrue(any(strcmp(sheets, 'Run1')));
asserts.isequal(format, 'Open XML Spreadsheet');
rmfile(filename);
%=============================================================================
