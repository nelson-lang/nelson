%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonzeroLengthText'), -1);
asserts.isequal(nargout('mustBeNonzeroLengthText'), 0);
%=============================================================================
mustBeNonzeroLengthText('a text');
mustBeNonzeroLengthText("a text");
asserts.checkerror('mustBeNonzeroLengthText(Inf)', sprintf(_('%s must contain text: a character vector, string array, or cell array of character vectors.'), 'Value'), 'Nelson:validators:mustBeNonzeroLengthText');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be non zero length text.'), 'Value')];
asserts.checkerror('mustBeNonzeroLengthText('''', 3)', msg, 'Nelson:validators:mustBeNonzeroLengthText:zeroLength');
%=============================================================================
