%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function result = getOptionField(value, name, defaultValue)
  result = defaultValue;
  if isfield(value, name)
    result = value.(name);
  end
end
%=============================================================================
