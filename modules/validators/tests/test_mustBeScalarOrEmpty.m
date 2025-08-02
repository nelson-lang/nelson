%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeScalarOrEmpty'), -1);
assert_isequal(nargout('mustBeScalarOrEmpty'), 0);
%=============================================================================
mustBeScalarOrEmpty(1);
mustBeScalarOrEmpty([]);
assert_checkerror('mustBeScalarOrEmpty([1 2])', _('Value must be scalar or empty.'), 'Nelson:validators:mustBeScalarOrEmpty');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be scalar or empty.')];
assert_checkerror('mustBeScalarOrEmpty([1 2], 3)', msg);
%=============================================================================
