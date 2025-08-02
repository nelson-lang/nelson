%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeVector'), -1);
assert_isequal(nargout('mustBeVector'), 0);
%=============================================================================
mustBeVector(3);
%=============================================================================
mustBeVector([1, 2]);
%=============================================================================
mustBeVector([1; 2]);
%=============================================================================
mustBeVector([], 'allows-all-empties');
%=============================================================================
assert_checkerror('mustBeVector([])', _('Value must be a vector.'), 'Nelson:validators:mustBeVector');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a vector.')];
assert_checkerror('mustBeVector(ones(4,5), 3)', msg, 'Nelson:validators:mustBeVector');
%=============================================================================
