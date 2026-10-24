%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function handle, a struct or a sparse array next to a string array has no
% string form, even when empty: the first one in argument order names the error.
%=============================================================================
id = 'Nelson:invalidConversion';
fmt = _('Conversion to string from %s is not possible.');
msgHandle = sprintf(fmt, 'function_handle');
msgStruct = sprintf(fmt, 'struct');
msgSparse = sprintf(fmt, 'sparse double');
%=============================================================================
assert_checkerror('[@sin, string.empty]', msgHandle, id);
assert_checkerror('[function_handle.empty, strings(0, 1)]', msgHandle, id);
assert_checkerror('[@sin, "a"]', msgHandle, id);
assert_checkerror('["a", @sin]', msgHandle, id);
assert_checkerror('[@sin; "a"]', msgHandle, id);
assert_checkerror('["a"; @sin]', msgHandle, id);
assert_checkerror('[string.empty, @sin]', msgHandle, id);
assert_checkerror('["a", function_handle.empty]', msgHandle, id);
assert_checkerror('["a", @sin; "b", "c"]', msgHandle, id);
assert_checkerror('horzcat("a", @sin)', msgHandle, id);
assert_checkerror('cat(1, "a", @sin)', msgHandle, id);
%=============================================================================
assert_checkerror('[struct(''a'', 1), "a"]', msgStruct, id);
assert_checkerror('["a", struct(''a'', 1)]', msgStruct, id);
assert_checkerror('["a"; struct(''a'', 1)]', msgStruct, id);
assert_checkerror('[struct([]), "a"]', msgStruct, id);
assert_checkerror('["a", struct([])]', msgStruct, id);
assert_checkerror('["a", struct.empty]', msgStruct, id);
assert_checkerror('vertcat("a", struct(''a'', 1))', msgStruct, id);
%=============================================================================
assert_checkerror('["a", sparse(1)]', msgSparse, id);
assert_checkerror('[sparse(1), "a"]', msgSparse, id);
assert_checkerror('["a"; sparse(1)]', msgSparse, id);
assert_checkerror('["a", sparse(1i)]', msgSparse, id);
assert_checkerror('["a", sparse([])]', msgSparse, id);
assert_checkerror('[string.empty, sparse([])]', msgSparse, id);
assert_checkerror('["a", sparse(true)]', sprintf(fmt, 'sparse logical'), id);
%=============================================================================
assert_checkerror('["a", sparse(1), @sin]', msgSparse, id);
assert_checkerror('["a", @sin, sparse(1)]', msgHandle, id);
assert_checkerror('["a", @sin, struct(''a'', 1)]', msgHandle, id);
assert_checkerror('["a", struct(''a'', 1), @sin]', msgStruct, id);
assert_checkerror('[1, "a", @sin]', msgHandle, id);
assert_checkerror('["a", {1}, @sin]', msgHandle, id);
%=============================================================================
R = ["a", {1}];
assert_isequal(R, ["a", "1"]);
R = ["a"; {1}];
assert_isequal(R, ["a"; "1"]);
R = ["a", zeros(0, 1)];
assert_isequal(R, "a");
R = ["a", int8(1), true];
assert_isequal(R, ["a", "1", "true"]);
%=============================================================================
