%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '_nelson_xlswrite_direct.xlsx'];
[status, message] = xlswrite(filename, [1 2; 3 4], 'Data', 'B2');
asserts.istrue(status, message);
R = readmatrix(filename, 'FileType', 'spreadsheet', 'Sheet', 'Data', 'Range', 'B2:C3');
asserts.isequal(R, [1 2; 3 4]);
rmfile(filename);
%=============================================================================
