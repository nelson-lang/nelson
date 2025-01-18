%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/286
% <-- Short Description -->
% fix [end] = sin(3)
%=============================================================================
assert_checkerror('[end] = sin(3)', _('Syntax error.'));
assert_checkerror('[1, end] = sin(3)', _('Syntax error.'));
msg = sprintf(_('Expecting %s'), _('statement list or function definition'));
assert_checkerror('end = sin(3)', msg);
%=============================================================================
assert_isequal(parsestring('– = 3'), 'error');
%=============================================================================
