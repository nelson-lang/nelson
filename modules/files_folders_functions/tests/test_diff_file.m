%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
msgA = ["hello"; "world A"];
msgB = ["hello"; "world B"];
fileA = [tempdir(), '/file_diff_A.txt'];
fileB = [tempdir(), '/file_diff_B.txt'];
filewrite(fileA, msgA)
filewrite(fileB, msgB)
%=============================================================================
R = diff_file(fileA, fileA);
assert_isequal(R, '')
%=============================================================================
R = diff_file(fileA, fileB, false);
REF = '@@ -1,2 +1,2 @@
hello
-world A
+world B
';
assert_isequal(replace(R, char([10 32]), char(10)) , replace(REF, char([10 32]), char(10)));
%=============================================================================
R = diff_file(fileB, fileA, false);
double(R)
REF = '@@ -1,2 +1,2 @@
hello
-world B
+world A
';
assert_isequal(replace(R, char([10 32]), char(10)) , replace(REF, char([10 32]), char(10)));
%=============================================================================
