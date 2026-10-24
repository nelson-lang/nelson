%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fread'), 3);
asserts.isequal(nargout('fread'), 1);
%=============================================================================
filename = [modulepath('characters_encoding', 'tests'), '/olaf_Windows-1251.txt'];
if ~iswasm()
F = fopen(filename, 'rt', 'n', 'windows-1251');
[R, S] = fread(F, '*char');
fclose(F);
asserts.isequal(S, 29106);
P = strfind(R, 'P.S. Когато преглеждах първата');
asserts.isequal(P, 14964);
end
%=============================================================================
% precision forms: '*type' shorthand, 'src=>dst', and 'type*N' block count
fn = [tempname(), '.bin'];
fw = fopen(fn, 'w');
fwrite(fw, 'hello', 'char*1');
fclose(fw);
fr = fopen(fn, 'r');
s = fread(fr, '*char');
fclose(fr);
asserts.isequal(reshape(s, 1, []), 'hello');
fr = fopen(fn, 'r');
s = fread(fr, 'char=>char');
fclose(fr);
asserts.isequal(reshape(s, 1, []), 'hello');
fw = fopen(fn, 'w');
fwrite(fw, uint8([104 105]), 'uint8');
fclose(fw);
fr = fopen(fn, 'r');
v = fread(fr, '*uint8');
fclose(fr);
asserts.isequal(class(v), 'uint8');
asserts.isequal(v, uint8([104; 105]));
% Fortran-style aliases keep working
fw = fopen(fn, 'w');
fwrite(fw, [1.5 2.5], 'real*8');
fclose(fw);
fr = fopen(fn, 'r');
v = fread(fr, Inf, 'real*8');
fclose(fr);
asserts.isequal(v, [1.5; 2.5]);
rmfile(fn);
%=============================================================================
