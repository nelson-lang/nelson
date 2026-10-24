%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeMatrix'), -1);
asserts.isequal(nargout('mustBeMatrix'), 0);
%=============================================================================
mustBeMatrix([]);
mustBeMatrix(single(3));
%=============================================================================
asserts.checkerror('mustBeMatrix(ones(2, 3, 2))', sprintf(_('%s must be a 2-D matrix.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a 2-D matrix.'), 'Value')];
asserts.checkerror('mustBeMatrix(ones(2, 3, 2), 3)', msg);
%=============================================================================

