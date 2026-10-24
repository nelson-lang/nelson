%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('aiask'), 2);
asserts.isequal(nargout('aiask'), -1);
%=============================================================================
opts = aioptions('Provider', 'unknown');
asserts.checkerror('aiask(''hello'', opts)', _('Unsupported AI provider.'));
%=============================================================================
% a non-text prompt is rejected before any provider request is attempted.
asserts.checkerror('aiask(5)', _('prompt must be a character vector or string scalar.'));
%=============================================================================
% a non-scalar string prompt is rejected.
asserts.checkerror('aiask(["a", "b"])', _('prompt must be a character vector or string scalar.'));
%=============================================================================
% a multi-row char prompt is rejected.
asserts.checkerror('aiask([''ab''; ''cd''])', _('prompt must be a character vector or string scalar.'));
%=============================================================================
% options must be a scalar struct.
asserts.checkerror('aiask(''hello'', 1)', _('options must be a scalar struct returned by aioptions.'));
%=============================================================================
