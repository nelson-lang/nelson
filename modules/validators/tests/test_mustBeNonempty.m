%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonempty'), -1);
assert_isequal(nargout('mustBeNonempty'), 0);
%=============================================================================
mustBeNonempty(1);
assert_checkerror('mustBeNonempty([])', _('Value must not be empty.'), 'Nelson:validators:mustBeNonempty');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must not be empty.')];
assert_checkerror('mustBeNonempty([], 3)', msg, 'Nelson:validators:mustBeNonempty');
%=============================================================================
