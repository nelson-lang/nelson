%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeLogical'), -1);
asserts.isequal(nargout('mustBeLogical'), 0);
%=============================================================================
mustBeLogical([]);
mustBeLogical([true, false]);
asserts.checkerror('mustBeLogical(1)', sprintf(_('%s must be logical.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be logical.'), 'Value')];
asserts.checkerror('mustBeLogical(1, 3)', msg);
%=============================================================================
