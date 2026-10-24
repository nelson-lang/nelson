%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('strlength'), 1);
asserts.isequal(nargout('strlength'), 1);
%=============================================================================
c = 'Hello, Mad World';
res = strlength(c);
ref = 16;
asserts.isequal(res, ref);
%=============================================================================
c = {'Hello, Mad World'};
res = strlength(c);
ref = 16;
asserts.isequal(res, ref);
%=============================================================================
c = {''};
res = strlength(c);
ref = 0;
asserts.isequal(res, ref);
%=============================================================================
c = '';
res = strlength(c);
ref = 0;
asserts.isequal(res, ref);
%=============================================================================
c = {};
res = strlength(c);
ref = [];
asserts.isequal(res, ref);
%=============================================================================
c = {'abs.docx', 'data.gz'; 'code.nlf', 'results.nlf'};
res = strlength(c);
ref = [8 7; 8 11];
asserts.isequal(res, ref);
%=============================================================================
cmd = 'res = strlength(1)';
asserts.checkerror(cmd, sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string or cell'));
%=============================================================================
B = ["Nel", NaN, "son"; "is", "open", "source"];
R = strlength(B);
REF = [3 NaN 3; 2 4 6];
asserts.isequal(R, REF);
%=============================================================================
% only character vectors (or '') are text: a character matrix, an empty
% matrix with a non-zero dimension or a cell holding such values is an error
% (the 3x0 case used to corrupt the heap).
msg = 'First argument must be a character vector, a string array or a cell array of character vectors.';
asserts.checkerror('R = strlength([''ab''; ''cd'']);', msg, 'Nelson:string:strlength:invalidInput');
asserts.checkerror('R = strlength(char(zeros(3, 0)));', msg, 'Nelson:string:strlength:invalidInput');
asserts.checkerror('R = strlength(char(zeros(0, 3)));', msg, 'Nelson:string:strlength:invalidInput');
asserts.checkerror('R = strlength({[''ab''; ''cd'']});', msg, 'Nelson:string:strlength:invalidInput');
asserts.checkerror('R = strlength({''ab'', 5});', 'Wrong type for argument #1: string or cell expected.', 'Nelson:invalidType');
asserts.checkerror('R = strlength(zeros(0, 3));', 'Wrong type for argument #1: string or cell expected.', 'Nelson:invalidType');
asserts.isequal(strlength(''), 0);
asserts.isequal(strlength('ab'), 2);
asserts.isequal(strlength({'ab'; 'cd'}), [2; 2]);
asserts.istrue(isnan(strlength(string(missing))));
%=============================================================================
