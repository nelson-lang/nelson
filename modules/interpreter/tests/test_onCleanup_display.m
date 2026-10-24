%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The separator lines of an onCleanup display are empty, without any blank.
% The line after the header stays in compact format.
%=============================================================================
currentFormat = format();
format('loose');
c = onCleanup(@() 1);
R = evalc('disp(c)');
assert_isequal(R, sprintf('  onCleanup with properties:\n\n    task: @()1\n\n'));
%=============================================================================
a = [onCleanup(@() 1), onCleanup(@() 2)];
REF = sprintf(['  1', char(215), '2 onCleanup array with properties:\n\n    task\n\n']);
R = evalc('disp(a)');
assert_isequal(R, REF);
%=============================================================================
format('compact');
R = evalc('disp(c)');
assert_isequal(R, sprintf('  onCleanup with properties:\n\n    task: @()1\n'));
R = evalc('disp(a)');
assert_isequal(R, REF);
%=============================================================================
format(currentFormat);
clear c a
%=============================================================================
