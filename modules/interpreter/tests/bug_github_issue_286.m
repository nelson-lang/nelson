%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/286
% <-- Short Description -->
% fix [end] = sin(3)
%=============================================================================
asserts.checkerror('[end] = sin(3)', _('Syntax error.'));
asserts.checkerror('[1, end] = sin(3)', _('Syntax error.'));
msg = strrep(_('Expecting %s'), '%s', _('statement list or function definition'));
asserts.checkerror('end = sin(3)', msg);
%=============================================================================
asserts.isequal(parsestring('â€“ = 3'), 'error');
%=============================================================================
