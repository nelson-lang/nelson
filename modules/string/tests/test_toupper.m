%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('toupper'), 1);
asserts.isequal(nargout('toupper'), 1);
%=============================================================================
A = 'Nelson';
R = toupper(A);
REF = 'NELSON';
asserts.isequal(R, REF);
%=============================================================================
A = {'r', 'b', 'C'};
R = toupper(A);
REF = {'R', 'B', 'C'};
asserts.isequal(R, REF);
%=============================================================================
A = ["r", "b", "C"];
R = toupper(A);
REF = ["R", "B", "C"];
asserts.isequal(R, REF);
%=============================================================================
% 2-D char matrix (multiple rows) is upper-cased element-wise, shape preserved
A = ['abc'; 'def'];
R = toupper(A);
asserts.isequal(R, ['ABC'; 'DEF']);
asserts.isequal(size(R), [2 3]);
%=============================================================================
% embedded NUL characters are preserved (no C-string truncation)
A = char([97 0 66]);
R = toupper(A);
asserts.isequal(double(R), [65 0 66]);
%=============================================================================
% column vector of chars
A = ['a'; 'b'; 'c'];
R = toupper(A);
asserts.isequal(R, ['A'; 'B'; 'C']);
%=============================================================================
% accented Latin letters are upper-cased (full Unicode case mapping)
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  A = char([233 224 231 232 234 235 239 252 246 244]);
  R = toupper(A);
  REF = char([201 192 199 200 202 203 207 220 214 212]);
  asserts.isequal(double(R), double(REF));
end
%=============================================================================
% Greek and Cyrillic letters are upper-cased
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  A = char([945 946 963 1072 1073]);
  R = toupper(A);
  REF = char([913 914 931 1040 1041]);
  asserts.isequal(double(R), double(REF));
end
%=============================================================================
% upper then lower restores the accented lowercase form (round-trip)
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  A = char([233 224 231]);
  asserts.isequal(double(tolower(toupper(A))), double(A));
end
%=============================================================================
