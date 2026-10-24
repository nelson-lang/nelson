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
asserts.isequal(nargin('convertCharsToStrings'), -1);
asserts.isequal(nargout('convertCharsToStrings'), -1);
%=============================================================================
[A, B, C, D] = convertCharsToStrings("one", 2, 'three', {'four'; 'five'});
asserts.isequal(A, "one");
asserts.isequal(B, 2);
asserts.isequal(C, "three");
asserts.isequal(D, ["four"; "five"]);
%=============================================================================
[A, B, C, D] = convertCharsToStrings("one", 2, 'three', {'four'; NaN; 'five'});
asserts.isequal(A, "one");
asserts.isequal(B, 2);
asserts.isequal(C, "three");
asserts.isequal(D, {'four'; NaN; 'five'});
%=============================================================================
[A, B, C, D] = convertCharsToStrings("one", 2, 'three', {'four'; 'NaN'; 'five'});
asserts.isequal(A, "one");
asserts.isequal(B, 2);
asserts.isequal(C, "three");
asserts.isequal(D, ["four"; "NaN"; "five"]);
%=============================================================================
R = convertCharsToStrings(char(ones(0, 3)));
asserts.isequal(R, "");
%=============================================================================
R = convertCharsToStrings({});
asserts.isequal(R, string([]));
%=============================================================================
R = convertCharsToStrings(['Nelson'; '  is  '; '  good']);
asserts.isequal(R, string('Nelson  is    good'));
%=============================================================================
