%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nelson.ai.ask'), 2);
asserts.isequal(nargout('nelson.ai.ask'), -1);
%=============================================================================
opts = nelson.ai.options('Provider', 'unknown');
asserts.checkerror('nelson.ai.ask(''hello'', opts)', _('Unsupported AI provider.'));
%=============================================================================
% a non-text prompt is rejected before any provider request is attempted.
asserts.checkerror('nelson.ai.ask(5)', _('prompt must be a character vector or string scalar.'));
%=============================================================================
% a non-scalar string prompt is rejected.
asserts.checkerror('nelson.ai.ask(["a", "b"])', _('prompt must be a character vector or string scalar.'));
%=============================================================================
% a multi-row char prompt is rejected.
asserts.checkerror('nelson.ai.ask([''ab''; ''cd''])', _('prompt must be a character vector or string scalar.'));
%=============================================================================
% options must be a scalar struct.
asserts.checkerror('nelson.ai.ask(''hello'', 1)', _('options must be a scalar struct returned by nelson.ai.options.'));
%=============================================================================
