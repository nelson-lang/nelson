%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
asserts.checkerror('xmldocrenderimages([], ''invalid_language'', true)', _('Invalid language'));
asserts.checkerror('xmldocrenderimages([], ''invalid_language'', true)', _('Invalid language'), 'Nelson:xmldocrenderimages:invalidLanguage');
asserts.checkerror('xmldocrenderimages('''', [], true)', _('Invalid module name.'));
asserts.checkerror('xmldocrenderimages('''', [], true)', _('Invalid module name.'), 'Nelson:xmldocrenderimages:invalidModuleName');
%=============================================================================
