%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeFolder'), -1);
assert_isequal(nargout('mustBeFolder'), 0);
%=============================================================================
mustBeFolder(tempdir());
assert_checkerror('mustBeFolder(Inf)', _('Value must be a character vector or string scalar.'), 'Nelson:validators:mustBeTextScalar');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be folder.')];
assert_checkerror('mustBeFolder(''Inf'', 3)', msg, 'Nelson:validators:mustBeFolder');
%=============================================================================
