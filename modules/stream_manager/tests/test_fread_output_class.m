%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Output class and shape of fread follow the precision: 'source=>output', '*source', 'source'.
filename = [tempname(), '.bin'];
fid = fopen(filename, 'w');
fwrite(fid, int16([1 -2 300]), 'int16');
fwrite(fid, uint8('abc'), 'uint8');
fclose(fid);
fid = fopen(filename, 'r');
%=============================================================================
R = fread(fid, 3, 'int16=>double');
asserts.isequal(R, [1; -2; 300]);
frewind(fid);
R = fread(fid, 3, '*int16');
asserts.isequal(R, int16([1; -2; 300]));
frewind(fid);
R = fread(fid, 3, 'int16');
asserts.isequal(R, [1; -2; 300]);
frewind(fid);
R = fread(fid, [1 3], 'int16');
asserts.isequal(R, [1 -2 300]);
frewind(fid);
R = fread(fid, 2, 'int16=>char');
asserts.isequal(class(R), 'char');
asserts.isequal(size(R), [2 1]);
asserts.isequal(double(R), [1; 0]);
%=============================================================================
fseek(fid, 6, 'bof');
[R, count] = fread(fid, '*char');
asserts.isequal(R, ['a'; 'b'; 'c']);
asserts.isequal(count, 3);
fseek(fid, 6, 'bof');
R = fread(fid, 'char=>char');
asserts.isequal(R, ['a'; 'b'; 'c']);
fseek(fid, 6, 'bof');
R = fread(fid, 'char');
asserts.isequal(R, double(['a'; 'b'; 'c']));
fseek(fid, 6, 'bof');
R = fread(fid, 'uint8=>char');
asserts.isequal(R, ['a'; 'b'; 'c']);
fseek(fid, 6, 'bof');
R = fread(fid, 'uint8');
asserts.isequal(R, double(['a'; 'b'; 'c']));
fseek(fid, 6, 'bof');
R = fread(fid, '*uint8');
asserts.isequal(R, uint8(['a'; 'b'; 'c']));
fseek(fid, 6, 'bof');
R = fread(fid);
asserts.isequal(R, double(['a'; 'b'; 'c']));
R = fread(fid, 'uint8');
asserts.isequal(R, zeros(0, 0));
%=============================================================================
fclose(fid);
delete(filename);
%=============================================================================
