%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Conversation history file for a surface's history scope, next to the other
% assistant preferences (permission mode, skills). The scope (see historyScope)
% maps the Qt dock and the web pane to one shared 'main' conversation, so the file
% name is always a safe fragment.
function f = historyFile(surfaceId)
  f = [historyDir(), '/conversation_', historyScope(surfaceId), '.json'];
end
%=============================================================================
