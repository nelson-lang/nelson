%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('aimodels'), 1);
asserts.isequal(nargout('aimodels'), -1);
%=============================================================================
opts = aioptions('Provider', 'unknown');
asserts.checkerror('aimodels(opts)', _('Unsupported AI provider.'));
%=============================================================================
opts = aioptions('Provider', 'openai-compatible', 'TokenEnvVar', 'NELSON_AI_MISSING_TOKEN_FOR_TEST');
asserts.checkerror('aimodels(opts)', _('AI provider token environment variable is not defined.'));
%=============================================================================
asserts.checkerror('aimodels(1)', _('options must be a scalar struct returned by aioptions.'));
%=============================================================================
% aimodels does not support the codex-cli provider (unlike aiask).
opts = aioptions('Provider', 'codex-cli');
asserts.checkerror('aimodels(opts)', _('Unsupported AI provider.'));
%=============================================================================
