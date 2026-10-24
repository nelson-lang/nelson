%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% Dynamic field width and precision taken from arguments with '*' must apply
% to numeric conversions, not only to '%s'. All expected values are the
% expected behavior.
%=============================================================================
% width from an argument
asserts.isequal(sprintf('%*d', 5, 42), '   42');
asserts.isequal(sprintf('%-*d', 5, 42), '42   ');
%=============================================================================
% precision from an argument
asserts.isequal(sprintf('%.*f', 3, pi), '3.142');
%=============================================================================
% both width and precision from arguments
asserts.isequal(sprintf('%*.*f', 10, 2, pi), '      3.14');
%=============================================================================
% flags combined with a dynamic width
asserts.isequal(sprintf('%0*d', 5, 42), '00042');
asserts.isequal(sprintf('%+*d', 6, 7), '    +7');
%=============================================================================
% hexadecimal and exponential with dynamic width/precision
asserts.isequal(sprintf('%*x', 4, 255), '  ff');
asserts.isequal(sprintf('%*.*e', 12, 3, 12345.6), '   1.235e+04');
%=============================================================================
% fprintf writes the same formatted text
str = evalc("fprintf('%*d', 5, 42)");
asserts.isequal(str, '   42');
%=============================================================================
