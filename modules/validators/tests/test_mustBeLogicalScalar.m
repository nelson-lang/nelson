%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeLogicalScalar'), -1);
asserts.isequal(nargout('mustBeLogicalScalar'), 0);
%=============================================================================
mustBeLogicalScalar(true);
%=============================================================================
asserts.checkerror('mustBeLogicalScalar(1)', sprintf(_('%s must be a logical scalar.'), 'Value'));
%=============================================================================
asserts.checkerror('mustBeLogicalScalar([true false])', sprintf(_('%s must be a logical scalar.'), 'Value'));
%=============================================================================
asserts.checkerror('mustBeLogicalScalar(logical([]))', sprintf(_('%s must be a logical scalar.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a logical scalar.'), 'Value')];
asserts.checkerror('mustBeLogicalScalar(1, 3)', msg);
%=============================================================================
