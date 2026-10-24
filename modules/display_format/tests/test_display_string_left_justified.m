%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% string array elements are left-justified in their column:
% a shorter element is completed with trailing blanks, last column included.
%=============================================================================
format('loose')
%=============================================================================
R = evalc('disp(["a" "bb"; "c" "d"])');
REF = sprintf('    "a"    "bb"\n    "c"    "d" \n\n');
assert_isequal(R, REF);
%=============================================================================
x = ["a" "bb"; "c" "d"];
R = evalc('x');
REF = sprintf('\nx = \n\n  2×2 string array\n\n    "a"    "bb"\n    "c"    "d" \n\n');
assert_isequal(R, REF);
%=============================================================================
y = ["a" "bb"; missing "d"];
R = evalc('y');
REF = sprintf('\ny = \n\n  2×2 string array\n\n    "a"          "bb"\n    <missing>    "d" \n\n');
assert_isequal(R, REF);
%=============================================================================
m = [missing "bb"; "c" missing];
R = evalc('disp(m)');
REF = sprintf('    <missing>    "bb"     \n    "c"          <missing>\n\n');
assert_isequal(R, REF);
%=============================================================================
z = ["" "bb"; "ccc" ""];
R = evalc('z');
REF = sprintf('\nz = \n\n  2×2 string array\n\n    ""       "bb"\n    "ccc"    ""  \n\n');
assert_isequal(R, REF);
%=============================================================================
p = cat(3, ["a" "bb"; "c" "d"], ["eee" "f"; "g" "hh"]);
R = evalc('p');
REF = sprintf(['\n  2×2×2 string array\n\n', ...
'p(:,:,1) = \n\n    "a"    "bb"\n    "c"    "d" \n\n\n', ...
'p(:,:,2) = \n\n    "eee"    "f" \n    "g"      "hh"\n\n']);
assert_isequal(R, REF);
%=============================================================================
R = evalc('disp(p)');
REF = sprintf(['\n(:,:,1) = \n\n    "a"    "bb"\n    "c"    "d" \n\n\n', ...
'(:,:,2) = \n\n    "eee"    "f" \n    "g"      "hh"\n\n']);
assert_isequal(R, REF);
%=============================================================================
format('compact')
R = evalc('x');
REF = sprintf('x = \n  2×2 string array\n    "a"    "bb"\n    "c"    "d" \n');
assert_isequal(R, REF);
R = evalc('p');
REF = sprintf(['  2×2×2 string array\n', ...
'p(:,:,1) = \n    "a"    "bb"\n    "c"    "d" \n', ...
'p(:,:,2) = \n    "eee"    "f" \n    "g"      "hh"\n']);
assert_isequal(R, REF);
format('loose')
%=============================================================================
