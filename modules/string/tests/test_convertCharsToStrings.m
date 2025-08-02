%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('convertCharsToStrings'), -1);
assert_isequal(nargout('convertCharsToStrings'), -1);
%=============================================================================
[A, B, C, D] = convertCharsToStrings("one", 2, 'three', {'four' ; 'five'});
assert_isequal(A, "one");
assert_isequal(B, 2);
assert_isequal(C, "three");
assert_isequal(D, ["four"; "five"]);
%=============================================================================
[A, B, C, D] = convertCharsToStrings("one", 2, 'three', {'four' ; NaN ; 'five'});
assert_isequal(A, "one");
assert_isequal(B, 2);
assert_isequal(C, "three");
assert_isequal(D, {'four' ; NaN ; 'five'});
%=============================================================================
[A, B, C, D] = convertCharsToStrings("one", 2, 'three', {'four' ; 'NaN' ;'five'});
assert_isequal(A, "one");
assert_isequal(B, 2);
assert_isequal(C, "three");
assert_isequal(D, ["four"; "NaN"; "five"]);
%=============================================================================
R = convertCharsToStrings(char(ones(0,3)));
assert_isequal(R, "");
%=============================================================================
R = convertCharsToStrings({});
assert_isequal(R, string([]));
%=============================================================================
R = convertCharsToStrings(['Nelson' ; '  is  '; '  good']);
assert_isequal(R, string('Nelson  is    good'));
%=============================================================================
