%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('tolower'), 1);
asserts.isequal(nargout('tolower'), 1);
%=============================================================================
A = 'Nelson';
R = tolower(A);
REF = 'nelson';
asserts.isequal(R, REF);
%=============================================================================
A = {'r', 'B', 'C'};
R = tolower(A);
REF = {'r', 'b', 'c'};
asserts.isequal(R, REF);
%=============================================================================
A = ["r", "b", string(NaN), "C"];
R = tolower(A);
REF = ["r", "b", string(NaN), "c"];
asserts.isequal(R, REF);
%=============================================================================
% 2-D char matrix (multiple rows) is lower-cased element-wise, shape preserved
A = ['ABC'; 'DEF'];
R = tolower(A);
asserts.isequal(R, ['abc'; 'def']);
asserts.isequal(size(R), [2 3]);
%=============================================================================
% embedded NUL characters are preserved (no C-string truncation)
A = char([65 0 98]);
R = tolower(A);
asserts.isequal(double(R), [97 0 98]);
%=============================================================================
% column vector of chars
A = ['A'; 'B'; 'C'];
R = tolower(A);
asserts.isequal(R, ['a'; 'b'; 'c']);
%=============================================================================
% accented Latin letters are lower-cased (full Unicode case mapping)
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  A = char([201 192 199 200 202 203 207 220 214 212]);
  R = tolower(A);
  REF = char([233 224 231 232 234 235 239 252 246 244]);
  asserts.isequal(double(R), double(REF));
end
%=============================================================================
% Greek and Cyrillic letters are lower-cased
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  A = char([913 914 931 1040 1041]);
  R = tolower(A);
  REF = char([945 946 963 1072 1073]);
  asserts.isequal(double(R), double(REF));
end
%=============================================================================
