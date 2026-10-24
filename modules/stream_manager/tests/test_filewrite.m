%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dest = [tempname(), '.txt'];
R1 = {['nel', char(10)], ['son', char(13), char(10)], ['soft', char(13), 'ware', char(10)]};
filewrite(dest, R1);
R2 = fileread(dest);
rmfile(dest);
%=============================================================================
dest = [tempname(), '.txt'];
R3 = ["Nelson"; "is"; "open"];
filewrite(dest, R3);
R4 = fileread(dest, "string");
asserts.isequal(R3, R4);
rmfile(dest);
%=============================================================================
dest = [tempname(), '.txt'];
str = 'живете зело, земля, и иже и како люди';
skip_testsuite(iswasm(), ...
  'WebAssembly reduced characters_encoding backend does not provide Windows-1251 conversion.');
filewrite(dest, str, 'native', 'windows-1251');
R = fileread(dest, 'char', 'native', 'windows-1251');
asserts.isequal(R, str);
rmfile(dest);
%=============================================================================
