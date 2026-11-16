%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nelson.ai.models'), 1);
asserts.isequal(nargout('nelson.ai.models'), -1);
%=============================================================================
opts = nelson.ai.options('Provider', 'unknown');
asserts.checkerror('nelson.ai.models(opts)', _('Unsupported AI provider.'));
%=============================================================================
opts = nelson.ai.options('Provider', 'openai-compatible', 'TokenEnvVar', 'NELSON_AI_MISSING_TOKEN_FOR_TEST');
asserts.checkerror('nelson.ai.models(opts)', _('AI provider token environment variable is not defined.'));
%=============================================================================
asserts.checkerror('nelson.ai.models(1)', _('options must be a scalar struct returned by nelson.ai.options.'));
%=============================================================================
% nelson.ai.models does not support the codex-cli provider (unlike nelson.ai.ask).
opts = nelson.ai.options('Provider', 'codex-cli');
asserts.checkerror('nelson.ai.models(opts)', _('Unsupported AI provider.'));
%=============================================================================
