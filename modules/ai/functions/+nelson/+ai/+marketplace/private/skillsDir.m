%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% User directory where installed assistant skills live (same one nelson.ai.skills
% reads from). Created on demand.
function d = skillsDir()
  d = [prefdir(), '/assistant/skills'];
  if ~isfolder(d)
    mkdir(d);
  end
end
%=============================================================================
