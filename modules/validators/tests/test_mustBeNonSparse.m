%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonSparse'), -1);
assert_isequal(nargout('mustBeNonSparse'), 0);
%=============================================================================
mustBeNonSparse(1);
assert_checkerror('mustBeNonSparse(sparse(0))', _('Value must not be sparse.'), 'Nelson:validators:mustBeNonSparse');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must not be sparse.')];
assert_checkerror('mustBeNonSparse(sparse(0), 3)', msg, 'Nelson:validators:mustBeNonSparse');
%=============================================================================
