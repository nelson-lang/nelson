%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeLogicalScalar'), -1);
assert_isequal(nargout('mustBeLogicalScalar'), 0);
%=============================================================================
mustBeLogicalScalar(true);
%=============================================================================
assert_checkerror('mustBeLogicalScalar(1)', _('Value must be logical scalar.'));
%=============================================================================
assert_checkerror('mustBeLogicalScalar([true false])', _('Value must be logical scalar.'));
%=============================================================================
assert_checkerror('mustBeLogicalScalar(logical([]))', _('Value must be logical scalar.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be logical scalar.')];
assert_checkerror('mustBeLogicalScalar(1, 3)', msg);
%=============================================================================
