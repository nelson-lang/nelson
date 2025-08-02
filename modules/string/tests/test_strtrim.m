%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('strtrim'), 1);
assert_isequal(nargout('strtrim'), 1);
%=============================================================================
TXT = sprintf(' \t\n\r\f\v');
R = strtrim(TXT);
REF = '';
assert_isequal(R, REF);
%=============================================================================
TXT = sprintf('  \t   AbCd   EfGg hijkl ... ');
R = strtrim(TXT);
REF = 'AbCd   EfGg hijkl ...';
assert_isequal(R, REF);
%=============================================================================
C = {'     A string with whitespace';
' Remove trailing whitespace ...       '};
R = strtrim(C);
REF = {'A string with whitespace';
'Remove trailing whitespace ...'};
assert_isequal(R, REF);
%=============================================================================
TXT = '     Test significant whitespace';
TXT = [TXT char(160) char(160) char(8199) char(8239) '   '];
R = strtrim(TXT);
REF = ['Test significant whitespace', char(160) char(160) char(8199) char(8239)];
assert_isequal(R, REF);
%=============================================================================
C = {'f ', 1};
assert_checkerror('R = strtrim(C);', _('cell of strings expected.'));
%=============================================================================
C = ["     A string with whitespace";
" Remove trailing whitespace ...       "];
R = strtrim(C);
REF = ["A string with whitespace";
"Remove trailing whitespace ..."];
assert_isequal(R, REF);
%=============================================================================
