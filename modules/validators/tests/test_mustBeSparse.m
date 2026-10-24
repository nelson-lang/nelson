%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeSparse'), -1);
asserts.isequal(nargout('mustBeSparse'), 0);
%=============================================================================
mustBeSparse(sparse([]));
mustBeSparse(sparse([1, 2]));
asserts.checkerror('mustBeSparse(Inf)', sprintf(_('%s must be a sparse matrix.'), 'Value'), 'Nelson:validators:mustBeSparse');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a sparse matrix.'), 'Value')];
asserts.checkerror('mustBeSparse(Inf, 3)', msg, 'Nelson:validators:mustBeSparse');
%=============================================================================
