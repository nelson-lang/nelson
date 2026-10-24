%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Locks the cross-cutting identifiers attached to the predefined argument-count
% messages raised directly (not through the shared nargincheck/nargoutcheck
% helpers). See modules/error_manager/docs/ERROR_IDENTIFIERS_CATALOG.md (Tier 5).
%=============================================================================
asserts.checkerror('triu(1, 2, 3)', _('Too many input arguments.'), 'Nelson:tooManyInputs');
%=============================================================================
asserts.checkerror('datenum(1, 2, 3, 4)', _('Failed to convert input to date numbers.'), 'Nelson:datenum:ConvertDateVector');
%=============================================================================
