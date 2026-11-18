%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% True when the AI assistant UI is enabled for this run. The assistant panels
% (console dock, editor and notebook helpers, NFlow build pane) are hidden by
% default and shown only when NELSON_ASSISTANT_AI is '1'; the nelson.ai.*
% functions stay callable either way. The Qt, web and NFlow front-ends read the
% same variable, so this accessor is the scriptable mirror of that gate.
function tf = isAssistantEnabled()
  tf = strcmp(strtrim(getenv('NELSON_ASSISTANT_AI')), '1');
end
%=============================================================================
