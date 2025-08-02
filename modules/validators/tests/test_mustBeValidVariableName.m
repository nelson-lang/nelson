%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeValidVariableName'), -1);
assert_isequal(nargout('mustBeValidVariableName'), 0);
%=============================================================================
mustBeValidVariableName('t8');
mustBeValidVariableName("t8");
%=============================================================================
assert_checkerror('mustBeValidVariableName(1)', _('Value must be valid variable name.'));
%=============================================================================
assert_checkerror('mustBeValidVariableName("8t")', _('Value must be valid variable name.'));
%=============================================================================
assert_checkerror('mustBeValidVariableName("8t")', _('Value must be valid variable name.'), 'Nelson:validators:mustBeValidVariableName');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be valid variable name.')];
assert_checkerror('mustBeValidVariableName("8t", 3)', msg);
%=============================================================================
