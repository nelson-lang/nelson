%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('blanks'), 1);
asserts.isequal(nargout('blanks'), 1);
%=============================================================================
r = blanks(3);
REF = '   ';
asserts.isequal(r, REF);
%=============================================================================
r = blanks(-3);
REF = char(zeros(1, 0));
asserts.isequal(r, REF);
%=============================================================================
asserts.checkerror('r = blanks([1 3]);', _('A scalar expected.'));
asserts.checkerror('r = blanks([1 3]);', _('A scalar expected.'), 'Nelson:types:scalarExpected');
%=============================================================================
asserts.checkerror('r = blanks(NaN);', _('NaN and Inf not allowed.'));
asserts.checkerror('r = blanks(NaN);', _('NaN and Inf not allowed.'), 'Nelson:string:nanInfNotAllowed');
%=============================================================================
asserts.checkerror('r = blanks(2.3);', sprintf(_('%s must be an integer.'), 'Value'));
%=============================================================================
