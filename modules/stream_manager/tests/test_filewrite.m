%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dest = [tempdir(), 'test_filewrite.txt'];
if isfile(dest)
  rmfile(dest);
end
R1 = {['nel', char(10)], ['son', char(13), char(10)], ['soft', char(13), 'ware', char(10)]};
filewrite(dest, R1);
R2 = fileread(dest);
%=============================================================================
dest = [tempdir(), 'test_filewrite_string.txt'];
if isfile(dest)
  rmfile(dest);
end
R3 = ["Nelson"; "is"; "open"];
filewrite(dest, R3);
R4 = fileread(dest, "string");
assert_isequal(R3, R4);
%=============================================================================
dest = [tempdir(), 'test_filewrite_encoding.txt'];
if isfile(dest)
  rmfile(dest);
end
str = 'живете зело, земля, и иже и како люди';
filewrite(dest, str, 'native', 'windows-1251');
R = fileread(dest,'char','native','windows-1251');
assert_isequal(R, str);
%=============================================================================
