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
asserts.isequal(nargin('deblank'), 1);
asserts.isequal(nargout('deblank'), 1);
%=============================================================================
TXT = sprintf(' \t\n\r\f\v');
R = deblank(TXT);
REF = '';
asserts.isequal(R, REF);
%=============================================================================
TXT = sprintf(' AbCd   EfGg hijkl ...   ');
R = deblank(TXT);
REF = ' AbCd   EfGg hijkl ...';
asserts.isequal(R, REF);
%=============================================================================
C = {'     A string with whitespace';
  ' Remove trailing whitespace ...       '};
R = deblank(C);
REF = {'     A string with whitespace';
  ' Remove trailing whitespace ...'};
asserts.isequal(R, REF);
%=============================================================================
TXT = '     Test significant whitespace';
TXT = [TXT char(160) char(160) char(8199) char(8239) '   '];
R = deblank(TXT);
REF = ['     Test significant whitespace', char(160) char(160) char(8199) char(8239)];
asserts.isequal(R, REF);
%=============================================================================
C = {'f ', 1};
asserts.isequal(deblank(C), {'f', 1});
%=============================================================================
C = ["     A string with whitespace";
  " Remove trailing whitespace ...       "];
R = deblank(C);
REF = ["     A string with whitespace";
  " Remove trailing whitespace ..."];
asserts.isequal(R, REF);
%=============================================================================
