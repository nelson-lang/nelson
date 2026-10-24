%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% loose line spacing drops the final newline of text values (string, char),
% compact keeps it; numeric values end with a newline in both.
%=============================================================================
format('default');
asserts.isequal(double(char(formattedDisplayText("a"))), 97);
asserts.isequal(double(char(formattedDisplayText('a'))), 97);
asserts.isequal(double(char(formattedDisplayText(['ab'; 'cd']))), [97 98 10 99 100]);
asserts.isequal(double(char(formattedDisplayText(" "))), 32);
asserts.isequal(formattedDisplayText(1), "     1" + newline);
asserts.isequal(formattedDisplayText(int8(3)), "   3" + newline);
asserts.isequal(formattedDisplayText("a", 'LineSpacing', 'compact'), "a" + newline);
asserts.isequal(formattedDisplayText(""), "");
asserts.isequal(formattedDisplayText(''), "");
asserts.isequal(formattedDisplayText(strings(0, 1)), "");
%=============================================================================
format('compact');
asserts.isequal(double(char(formattedDisplayText("a"))), [97 10]);
asserts.isequal(double(char(formattedDisplayText('a'))), [97 10]);
asserts.isequal(double(char(formattedDisplayText(['ab'; 'cd']))), [97 98 10 99 100 10]);
asserts.isequal(formattedDisplayText(1), "     1" + newline);
asserts.isequal(formattedDisplayText("a", 'LineSpacing', 'loose'), "a");
asserts.isequal(formattedDisplayText(""), "");
asserts.isequal(formattedDisplayText(''), "");
asserts.isequal(evalc('disp("")'), '');
format('default');
%=============================================================================
asserts.isequal(evalc('disp("")'), '');
asserts.isequal(evalc('disp(" ")'), [' ', newline]);
%=============================================================================
