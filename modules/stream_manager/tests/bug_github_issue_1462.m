%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1462
% <-- Short Description -->
% fwrite without specifying precision does not work as expected.
%=============================================================================
test_string =  'é ö ä ü è ê';
fid = fopen([tempdir(), 'bug_#1462.txt'],'wt','n', 'UTF-8');
count = fwrite(fid, test_string);
fclose(fid);
%=============================================================================
assert_isequal(count, 11);
%=============================================================================
R = fileread([tempdir(), 'bug_#1462.txt']);
assert_isequal(R, test_string);
%=============================================================================
test_string =  'é ö ä ü è ê';
fid = fopen([tempdir(), 'bug_#1462.txt'],'wt','n', 'UTF-8');
[count, bytes] = fwrite(fid, test_string);
fclose(fid);
%=============================================================================
assert_isequal(count, 11);
assert_isequal(bytes, 17);
%=============================================================================
