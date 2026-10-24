%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Bug 5: fscanf '%f' must be read as a double (was single -> precision loss).
f = [tempname(), '.txt'];
filewrite(f, '3.14');
fid = fopen(f, 'r');
R = fscanf(fid, '%f');
fclose(fid);
rmfile(f);
asserts.isequal(R, 3.14);
%=============================================================================
f = [tempname(), '.txt'];
filewrite(f, '0.30000000000000004');
fid = fopen(f, 'r');
R = fscanf(fid, '%f');
fclose(fid);
rmfile(f);
asserts.isequal(R, 0.30000000000000004);
%=============================================================================
% Bug 2: fscanf '%s' token longer than the old fixed 8192-byte stack buffer
% used to overflow the stack. It must now read without crashing.
BIG = repmat('a', 1, 100000);
f = [tempname(), '.txt'];
filewrite(f, BIG);
fid = fopen(f, 'r');
R = fscanf(fid, '%s');
fclose(fid);
rmfile(f);
asserts.isequal(numel(R), 100000);
%=============================================================================
% Bug 1: fscanf '%c' reads all characters (whitespace included).
f = [tempname(), '.txt'];
filewrite(f, 'hello world');
fid = fopen(f, 'r');
R = fscanf(fid, '%c');
fclose(fid);
rmfile(f);
asserts.isequal(R, 'hello world');
%=============================================================================
