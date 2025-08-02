%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('convertStringsToChars'), -1);
assert_isequal(nargout('convertStringsToChars'), -1);
%=============================================================================
[A, B, C, D] = convertStringsToChars('one', 2, "three", ["four" ; "five"]);
assert_isequal(A, 'one');
assert_isequal(B, 2);
assert_isequal(C, 'three');
assert_isequal(D, {'four'; 'five'});
%=============================================================================
[A, B, C, D] = convertStringsToChars('one', 2, "three", ["four" ; string(NaN) ; "five"]);
assert_isequal(A, 'one');
assert_isequal(B, 2);
assert_isequal(C, 'three');
assert_isequal(D, {'four'; ''; 'five'});
%=============================================================================
[A, B, C, D] = convertStringsToChars('one', 2, string(NaN), ["four" ; string(NaN) ; "five"]);
assert_isequal(A, 'one');
assert_isequal(B, 2);
assert_isequal(C, '');
assert_isequal(D, {'four'; ''; 'five'});
%=============================================================================
R = convertStringsToChars(string([]));
assert_isequal(R, {});
%=============================================================================
