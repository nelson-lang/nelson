%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '_nelson_xlsread_direct.xlsx'];
[status, message] = xlswrite(filename, [7 8; 9 10], 'Data', 'C3');
asserts.istrue(status, message);
[num, txt, raw] = xlsread(filename, 'Data', 'C3:D4');
asserts.isequal(num, [7 8; 9 10]);
asserts.isequal(txt, cell(2, 2));
asserts.isequal(raw, {7, 8; 9, 10});
rmfile(filename);
%=============================================================================
