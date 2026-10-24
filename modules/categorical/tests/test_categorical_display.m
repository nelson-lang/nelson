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
% categorical display: size line, elements left-justified to the column
% width, one trailing blank per element
%=============================================================================
LF = char(10);
X = char(215);
format loose
%=============================================================================
g = categorical({'a';'b'});
REF = [LF, 'g = ', LF, LF, '  2', X, '1 categorical array', LF, LF, '     a ', LF, '     b ', LF, LF];
asserts.isequal(evalc('g'), REF);
%=============================================================================
s = categorical({'hello'});
REF = [LF, 's = ', LF, LF, '  categorical', LF, LF, '     hello ', LF, LF];
asserts.isequal(evalc('s'), REF);
%=============================================================================
r = categorical({'a','bbb','cc'});
REF = [LF, 'r = ', LF, LF, '  1', X, '3 categorical array', LF, LF, '     a      bbb      cc ', LF, LF];
asserts.isequal(evalc('r'), REF);
%=============================================================================
m = categorical({'a','bbb';'cccc',''});
REF = [LF, 'm = ', LF, LF, '  2', X, '2 categorical array', LF, LF, ...
'     a         bbb         ', LF, ...
'     cccc      <undefined> ', LF, LF];
asserts.isequal(evalc('m'), REF);
%=============================================================================
% one column: no padding to the column width
v = categorical({'longword';'b'});
REF = [LF, 'v = ', LF, LF, '  2', X, '1 categorical array', LF, LF, '     longword ', LF, '     b ', LF, LF];
asserts.isequal(evalc('v'), REF);
%=============================================================================
q = categorical([1 2;3 4]);
REF = [LF, 'q = ', LF, LF, '  2', X, '2 categorical array', LF, LF, '     1      2 ', LF, '     3      4 ', LF, LF];
asserts.isequal(evalc('q'), REF);
%=============================================================================
n = categorical({'a','b';'c','d'});
n(:,:,2) = categorical({'e','ff';'g','h'});
PAGES = [LF, 'n(:,:,1) = ', LF, LF, '     a      b ', LF, '     c      d ', LF, LF, ...
LF, 'n(:,:,2) = ', LF, LF, '     e      ff ', LF, '     g      h  ', LF, LF];
REF = [LF, '  2', X, '2', X, '2 categorical array', LF, PAGES];
asserts.isequal(evalc('n'), REF);
%=============================================================================
asserts.isequal(evalc('disp(g)'), ['     a ', LF, '     b ', LF, LF]);
asserts.isequal(evalc('disp(s)'), ['     hello ', LF, LF]);
asserts.isequal(evalc('disp(n)'), strrep(PAGES, 'n(', '('));
%=============================================================================
REF = ['  1', X, '2 categorical array', LF, LF, '     a      b ', LF, LF];
asserts.isequal(evalc('display(categorical({''a'',''b''}))'), REF);
%=============================================================================
format compact
R1 = evalc('g');
R2 = evalc('n');
R3 = evalc('disp(g)');
R4 = evalc('s');
format loose
asserts.isequal(R1, ['g = ', LF, '  2', X, '1 categorical array', LF, '     a ', LF, '     b ', LF]);
REF = ['  2', X, '2', X, '2 categorical array', LF, ...
'n(:,:,1) = ', LF, '     a      b ', LF, '     c      d ', LF, ...
'n(:,:,2) = ', LF, '     e      ff ', LF, '     g      h  ', LF];
asserts.isequal(R2, REF);
asserts.isequal(R3, ['     a ', LF, '     b ', LF]);
asserts.isequal(R4, ['s = ', LF, '  categorical', LF, '     hello ', LF]);
%=============================================================================
z = categorical(zeros(2,2,0));
REF = [LF, 'z = ', LF, LF, '  2', X, '2', X, '0 empty categorical array', LF, LF];
asserts.isequal(evalc('z'), REF);
%=============================================================================
% pages without a name start with a blank line
PAGES = [LF, '(:,:,1) = ', LF, LF, '     a      b ', LF, LF, ...
LF, '(:,:,2) = ', LF, LF, '     c      d ', LF, LF];
REF = [LF, '  1', X, '2', X, '2 categorical array', LF, PAGES];
asserts.isequal(evalc('display(reshape(categorical({''a'',''b'',''c'',''d''}),[1 2 2]))'), REF);
%=============================================================================
% evalc width is 80: a row splits in column groups before reaching it
w = categorical(1:15);
ROW1 = ['     1      2      3      4      5      6      7      8      9      10      11 ', LF];
ROW2 = ['     12      13      14      15 ', LF];
BODY = ['  Columns 1 through 11', LF, LF, ROW1, LF, '  Columns 12 through 15', LF, LF, ROW2, LF];
REF = [LF, 'w = ', LF, LF, '  1', X, '15 categorical array', LF, LF, BODY];
asserts.isequal(evalc('w'), REF);
asserts.isequal(evalc('disp(w)'), BODY);
%=============================================================================
% 79 characters fit, 80 do not
names = cellstr([repmat('a', 3, 13), ('1':'3')'])';
e79 = categorical([names, {repmat('b', 1, 13)}]);
R = evalc('e79');
asserts.isequal(numel(strfind(R, 'Column')), 0);
e80 = categorical([names, {repmat('b', 1, 14)}]);
R = evalc('e80');
asserts.isequal(numel(strfind(R, 'Columns 1 through 3')), 1);
asserts.isequal(numel(strfind(R, 'Column 4')), 1);
%=============================================================================
% long names cut to max(19, floor(width / columns) - 7) with an ellipsis,
% a single column is never cut
x100 = repmat('x', 1, 100);
t = categorical({x100, 'y'});
REF = [LF, 't = ', LF, LF, '  1', X, '2 categorical array', LF, LF, ...
'     ', repmat('x', 1, 32), char(8230), '      y ', LF, LF];
asserts.isequal(evalc('t'), REF);
t = categorical({x100; 'y'});
REF = [LF, 't = ', LF, LF, '  2', X, '1 categorical array', LF, LF, '     ', x100, ' ', LF, '     y ', LF, LF];
asserts.isequal(evalc('t'), REF);
t = categorical({x100, x100, x100, x100, x100, x100});
R = evalc('t');
asserts.isequal(numel(strfind(R, [repmat('x', 1, 18), char(8230), ' '])), 6);
asserts.isequal(numel(strfind(R, 'Columns 1 through 3')), 1);
%=============================================================================
% rows of a matrix split together, each page split on its own
m = categorical(reshape(1:24, 2, 12));
REF = [LF, 'm = ', LF, LF, '  2', X, '12 categorical array', LF, LF, ...
'  Columns 1 through 10', LF, LF, ...
'     1      3      5      7      9       11      13      15      17      19 ', LF, ...
'     2      4      6      8      10      12      14      16      18      20 ', LF, LF, ...
'  Columns 11 through 12', LF, LF, ...
'     21      23 ', LF, ...
'     22      24 ', LF, LF];
asserts.isequal(evalc('m'), REF);
n3 = categorical(reshape(1:24, 1, 12, 2));
REF = [LF, '  1', X, '12', X, '2 categorical array', LF, ...
LF, 'n3(:,:,1) = ', LF, LF, '  Columns 1 through 11', LF, LF, ...
'     1      2      3      4      5      6      7      8      9      10      11 ', LF, LF, ...
'  Column 12', LF, LF, '     12 ', LF, LF, ...
LF, 'n3(:,:,2) = ', LF, LF, '  Columns 1 through 9', LF, LF, ...
'     13      14      15      16      17      18      19      20      21 ', LF, LF, ...
'  Columns 10 through 12', LF, LF, '     22      23      24 ', LF, LF];
asserts.isequal(evalc('n3'), REF);
%=============================================================================
format compact
R = evalc('w');
format loose
REF = ['w = ', LF, '  1', X, '15 categorical array', LF, '  Columns 1 through 11', LF, ROW1, ...
'  Columns 12 through 15', LF, ROW2];
asserts.isequal(R, REF);
%=============================================================================
