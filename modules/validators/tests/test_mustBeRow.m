%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeRow'), -1);
asserts.isequal(nargout('mustBeRow'), 0);
%=============================================================================
mustBeRow('Nelson');
mustBeRow([1, 2, 3]);
%=============================================================================
asserts.checkerror('mustBeRow([])', sprintf(_('%s must be a row vector.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a row vector.'), 'Value')];
asserts.checkerror('mustBeRow([], 3)', msg);
%=============================================================================

