%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeFinite'), -1);
assert_isequal(nargout('mustBeFinite'), 0);
%=============================================================================
mustBeFinite(1);
assert_checkerror('mustBeFinite(Inf)', _('Value must be finite.'), 'Nelson:validators:mustBeFinite');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be finite.')];
assert_checkerror('mustBeFinite(Inf, 3)', msg, 'Nelson:validators:mustBeFinite');
%=============================================================================
