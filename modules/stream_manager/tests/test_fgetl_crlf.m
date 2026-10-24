%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fgetl must strip the whole line terminator, including a Windows CRLF.
%=============================================================================
crlf = char([13 10]);
filename = [tempdir(), 'test_fgetl_crlf.txt'];
fid = fopen(filename, 'wb');
fwrite(fid, ['hello', crlf, 'world', crlf]);
fclose(fid);
%=============================================================================
fid = fopen(filename, 'r');
l1 = fgetl(fid);
l2 = fgetl(fid);
fclose(fid);
%=============================================================================
asserts.isequal(l1, 'hello');
asserts.isequal(length(l1), 5);
asserts.isfalse(any(double(l1) == 13));
asserts.isequal(l2, 'world');
asserts.isequal(length(l2), 5);
asserts.isfalse(any(double(l2) == 13));
%=============================================================================
% A bare LF terminator must keep working.
fid = fopen(filename, 'wb');
fwrite(fid, ['foo', char(10), 'bar', char(10)]);
fclose(fid);
fid = fopen(filename, 'r');
l1 = fgetl(fid);
l2 = fgetl(fid);
fclose(fid);
asserts.isequal(l1, 'foo');
asserts.isequal(l2, 'bar');
%=============================================================================
rmfile(filename);
%=============================================================================
