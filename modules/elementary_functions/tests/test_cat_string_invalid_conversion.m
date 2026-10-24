%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% cat along dim > 2 next to a string array: a function handle, a struct or a
% sparse array has no string form, even when empty or 0x0, and the first one
% in argument order names the error, as with [] concatenation.
%=============================================================================
id = 'Nelson:invalidConversion';
fmt = _('Conversion to string from %s is not possible.');
msgHandle = sprintf(fmt, 'function_handle');
msgStruct = sprintf(fmt, 'struct');
msgSparse = sprintf(fmt, 'sparse double');
%=============================================================================
assert_checkerror('cat(3, @sin, "s")', msgHandle, id);
assert_checkerror('cat(3, "s", @sin)', msgHandle, id);
assert_checkerror('cat(4, @sin, "s")', msgHandle, id);
assert_checkerror('cat(3, @sin, string.empty)', msgHandle, id);
assert_checkerror('cat(3, string.empty, @sin)', msgHandle, id);
assert_checkerror('cat(3, function_handle.empty, "s")', msgHandle, id);
assert_checkerror('cat(3, function_handle.empty, string.empty)', msgHandle, id);
assert_checkerror('cat(3, string.empty, function_handle.empty)', msgHandle, id);
assert_checkerror('cat(3, @sin, strings(0, 1))', msgHandle, id);
assert_checkerror('cat(3, @sin, strings(1, 1, 0))', msgHandle, id);
assert_checkerror('cat(3, @sin, "s", @cos)', msgHandle, id);
assert_checkerror('cat(3, ["a", "b"], @sin)', msgHandle, id);
assert_checkerror('cat(3, @sin, zeros(1, 1, 0), "s")', msgHandle, id);
assert_checkerror('cat(3, 1, @sin, "s")', msgHandle, id);
assert_checkerror('cat(3, @sin, 1, "s")', msgHandle, id);
%=============================================================================
assert_checkerror('cat(3, struct(''a'', 1), "s")', msgStruct, id);
assert_checkerror('cat(3, "s", struct(''a'', 1))', msgStruct, id);
assert_checkerror('cat(3, "s", struct([]))', msgStruct, id);
assert_checkerror('cat(3, struct([]), "s")', msgStruct, id);
assert_checkerror('cat(3, string.empty, struct([]))', msgStruct, id);
%=============================================================================
assert_checkerror('cat(3, "s", sparse(1))', msgSparse, id);
assert_checkerror('cat(3, sparse(1), "s")', msgSparse, id);
assert_checkerror('cat(3, "s", sparse([]))', msgSparse, id);
%=============================================================================
assert_checkerror('cat(3, "s", @sin, struct(''a'', 1))', msgHandle, id);
assert_checkerror('cat(3, "s", struct(''a'', 1), @sin)', msgStruct, id);
assert_checkerror('cat(3, "s", sparse(1), @sin)', msgSparse, id);
%=============================================================================
R = cat(3, "s", 1);
assert_isequal(class(R), 'string');
assert_isequal(size(R), [1 1 2]);
R = cat(3, "s", {1});
assert_isequal(class(R), 'string');
assert_isequal(size(R), [1 1 2]);
%=============================================================================
