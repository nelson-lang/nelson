%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeVector'), -1);
asserts.isequal(nargout('mustBeVector'), 0);
%=============================================================================
mustBeVector(3);
%=============================================================================
mustBeVector([1, 2]);
%=============================================================================
mustBeVector([1; 2]);
%=============================================================================
mustBeVector([], 'allows-all-empties');
%=============================================================================
asserts.checkerror('mustBeVector([])', sprintf(_('%s must be a vector.'), 'Value'), 'Nelson:validators:mustBeVector');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a vector.'), 'Value')];
asserts.checkerror('mustBeVector(ones(4,5), 3)', msg, 'Nelson:validators:mustBeVector');
%=============================================================================
