%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Bug 1: '%c' without width used to crash (stack buffer[-1] = 0 on EOF).
% Expected behavior reads ALL remaining characters (whitespace included) as a char row.
R = sscanf('42', '%c');
asserts.isequal(R, '42');
asserts.isequal(class(R), 'char');
%=============================================================================
R = sscanf('  42', '%c');
asserts.isequal(R, '  42');
%=============================================================================
% Multiple '%c' conversions (used to crash on the trailing EOF read).
R = sscanf('ab', '%c%c%c%c');
asserts.isequal(R, 'ab');
asserts.isequal(class(R), 'char');
%=============================================================================
% Bug 2: '%s' token longer than the old fixed 8192-byte stack buffer used to
% overflow the stack. It must now read without crashing.
BIG = repmat('a', 1, 100000);
R = sscanf(BIG, '%s');
asserts.isequal(numel(R), 100000);
asserts.isequal(R, BIG);
%=============================================================================
% Bug 5: '%f' must be read as a double (was read as single -> precision loss).
R = sscanf('3.14', '%f');
asserts.isequal(R, 3.14);
%=============================================================================
R = sscanf('0.30000000000000004', '%f');
asserts.isequal(R, 0.30000000000000004);
%=============================================================================
% A value not exactly representable in single precision must survive.
R = sscanf('0.1', '%f');
asserts.isequal(R, 0.1);
%=============================================================================
