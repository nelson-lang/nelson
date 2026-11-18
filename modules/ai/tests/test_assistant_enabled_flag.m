%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The assistant UI gate: nelson.ai.isAssistantEnabled() mirrors NELSON_ASSISTANT_AI,
% on only for '1' (off by default, off for any other value), so the front-ends and
% scripts read one source of truth.
previous = getenv('NELSON_ASSISTANT_AI');
restore = onCleanup(@() setenv('NELSON_ASSISTANT_AI', previous)); %#ok<NASGU>
%=============================================================================
setenv('NELSON_ASSISTANT_AI', '');
assert_isfalse(nelson.ai.isAssistantEnabled());
%=============================================================================
setenv('NELSON_ASSISTANT_AI', '0');
assert_isfalse(nelson.ai.isAssistantEnabled());
%=============================================================================
setenv('NELSON_ASSISTANT_AI', '1');
assert_istrue(nelson.ai.isAssistantEnabled());
%=============================================================================
setenv('NELSON_ASSISTANT_AI', ' 1 ');
assert_istrue(nelson.ai.isAssistantEnabled());
%=============================================================================
setenv('NELSON_ASSISTANT_AI', 'true');
assert_isfalse(nelson.ai.isAssistantEnabled());
%=============================================================================
