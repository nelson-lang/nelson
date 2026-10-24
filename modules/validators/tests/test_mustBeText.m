%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeText'), -1);
asserts.isequal(nargout('mustBeText'), 0);
%=============================================================================
mustBeText('a text');
mustBeText("a text");
asserts.checkerror('mustBeText(Inf)', sprintf(_('%s must contain text: a character vector, string array, or cell array of character vectors.'), 'Value'), 'Nelson:validators:mustBeText');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must contain text: a character vector, string array, or cell array of character vectors.'), 'Value')];
asserts.checkerror('mustBeText(Inf, 3)', msg, 'Nelson:validators:mustBeText');
%=============================================================================
