%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeText'), -1);
assert_isequal(nargout('mustBeText'), 0);
%=============================================================================
mustBeText('a text');
mustBeText("a text");
assert_checkerror('mustBeText(Inf)', _('Value must be a character vector, string array or cell array of character vectors.'), 'Nelson:validators:mustBeText');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a character vector, string array or cell array of character vectors.')];
assert_checkerror('mustBeText(Inf, 3)', msg, 'Nelson:validators:mustBeText');
%=============================================================================
