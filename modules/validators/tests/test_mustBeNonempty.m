%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonempty'), -1);
asserts.isequal(nargout('mustBeNonempty'), 0);
%=============================================================================
mustBeNonempty(1);
asserts.checkerror('mustBeNonempty([])', sprintf(_('%s must be nonempty.'), 'Value'), 'Nelson:validators:mustBeNonempty');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be nonempty.'), 'Value')];
asserts.checkerror('mustBeNonempty([], 3)', msg, 'Nelson:validators:mustBeNonempty');
%=============================================================================
