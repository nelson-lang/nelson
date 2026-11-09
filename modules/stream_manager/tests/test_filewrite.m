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
% LF and CR LF inside a line become the end of line, a lone CR is kept
dest = [tempname(), '.txt'];
LF = char(10);
CR = char(13);
filewrite(dest, {['nel', LF], ['son', CR, LF], ['soft', CR, 'ware', LF]}, 'unix');
asserts.isequal(readText(dest), ['nel', LF, 'son', LF, 'soft', CR, 'ware', LF]);
filewrite(dest, {['nel', LF], ['son', CR, LF], ['soft', CR, 'ware', LF]}, 'pc');
asserts.isequal(readText(dest), ['nel', CR, LF, 'son', CR, LF, 'soft', CR, 'ware', CR, LF]);
filewrite(dest, ["a"; "b"], 'unix');
asserts.isequal(readText(dest), ['a', LF, 'b']);
filewrite(dest, ['ab'; 'cd'], 'unix');
asserts.isequal(readText(dest), ['ab', LF, 'cd']);
filewrite(dest, ["a"; string(NaN); "b"], 'unix');
asserts.isequal(readText(dest), ['a', LF, LF, 'b']);
filewrite(dest, ['a', CR, CR, LF, 'b'], 'unix');
asserts.isequal(readText(dest), ['a', CR, LF, 'b']);
filewrite(dest, '');
asserts.isequal(readText(dest), char(zeros(1, 0)));
filewrite(dest, string(char([233, 8364])));
asserts.isequal(readText(dest), char([195 169 226 130 172]));
asserts.checkerror('filewrite(dest, {''a'', 1})', _('A cell of string expected.'));
asserts.checkerror('filewrite(dest, 1)', _('A cell or string array expected.'));
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
function t = readText(filename)
  fid = fopen(filename, 'rb');
  t = char(reshape(double(fread(fid, Inf, 'uint8')), 1, []));
  fclose(fid);
end
%=============================================================================
