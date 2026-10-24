%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNumeric'), -1);
asserts.isequal(nargout('mustBeNumeric'), 0);
%=============================================================================
mustBeNumeric([])
mustBeNumeric([1, 2]);
asserts.checkerror('mustBeNumeric({1})', sprintf(_('%s must be numeric.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be numeric.'), 'Value')];
asserts.checkerror('mustBeNumeric({}, 3)', msg);
%=============================================================================


