%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeSparse'), -1);
assert_isequal(nargout('mustBeSparse'), 0);
%=============================================================================
mustBeSparse(sparse([]));
mustBeSparse(sparse([1, 2]));
assert_checkerror('mustBeSparse(Inf)', _('Value must be a sparse matrix.'), 'Nelson:validators:mustBeSparse');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a sparse matrix.')];
assert_checkerror('mustBeSparse(Inf, 3)', msg, 'Nelson:validators:mustBeSparse');
%=============================================================================
