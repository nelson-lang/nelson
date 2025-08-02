%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonzeroLengthText'), -1);
assert_isequal(nargout('mustBeNonzeroLengthText'), 0);
%=============================================================================
mustBeNonzeroLengthText('a text');
mustBeNonzeroLengthText("a text");
assert_checkerror('mustBeNonzeroLengthText(Inf)', _('Value must be a character vector, string array or cell array of character vectors.'), 'Nelson:validators:mustBeNonzeroLengthText');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be non zero length text.')];
assert_checkerror('mustBeNonzeroLengthText('''', 3)', msg, 'Nelson:validators:mustBeNonzeroLengthText');
%=============================================================================
