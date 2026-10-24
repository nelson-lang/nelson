%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(strip('  abc  '), 'abc');
asserts.isequal(strip('__abc__', 'both', '_'), 'abc');
asserts.isequal(strip(["  a", "b  "]), ["a", "b"]);
%=============================================================================
s = ["  a", missing, "b  "];
r = strip(s);
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "a");
asserts.isequal(r(3), "b");
%=============================================================================
% side in any case, strip character, Unicode space separators.
asserts.isequal(strip('  ab  ', 'LEFT'), 'ab  ');
asserts.isequal(strip('xxabxx', 'left', 'x'), 'abxx');
asserts.isequal(strip('xxabxx', 'right', 'x'), 'xxab');
asserts.isequal(strip('xxabxx', 'x'), 'ab');
asserts.isequal(strip(char([9 97 10])), 'a');
asserts.isequal(strip(char([160 97 160])), [char(160), 'a', char(160)]);
asserts.isequal(strip(char([8192 97 8192])), 'a');
asserts.isequal(strip(' '), '');
asserts.isequal(strip(''), '');
%=============================================================================
