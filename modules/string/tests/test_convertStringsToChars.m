%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('convertStringsToChars'), -1);
asserts.isequal(nargout('convertStringsToChars'), -1);
%=============================================================================
[A, B, C, D] = convertStringsToChars('one', 2, "three", ["four"; "five"]);
asserts.isequal(A, 'one');
asserts.isequal(B, 2);
asserts.isequal(C, 'three');
asserts.isequal(D, {'four'; 'five'});
%=============================================================================
[A, B, C, D] = convertStringsToChars('one', 2, "three", ["four"; string(NaN); "five"]);
asserts.isequal(A, 'one');
asserts.isequal(B, 2);
asserts.isequal(C, 'three');
asserts.isequal(D, {'four'; ''; 'five'});
%=============================================================================
[A, B, C, D] = convertStringsToChars('one', 2, string(NaN), ["four"; string(NaN); "five"]);
asserts.isequal(A, 'one');
asserts.isequal(B, 2);
asserts.isequal(C, '');
asserts.isequal(D, {'four'; ''; 'five'});
%=============================================================================
R = convertStringsToChars(string([]));
asserts.isequal(R, {});
%=============================================================================
