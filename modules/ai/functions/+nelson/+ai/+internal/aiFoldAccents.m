%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Lower-case a string and fold common Latin accents to ASCII, so a French prompt
% ("réponse", "résistance") tokenizes to the same [a-z] words as the English
% metadata it is matched against. Code points are used directly, so this source
% file stays pure ASCII.
function out = aiFoldAccents(text)
  narginchk(1, 1);
  out = lower(char(text));
  if isempty(out)
    return
  end
  groups = { ...
    'a', [224, 225, 226, 227, 228, 229]; ...
    'e', [232, 233, 234, 235]; ...
    'i', [236, 237, 238, 239]; ...
    'o', [242, 243, 244, 245, 246]; ...
    'u', [249, 250, 251, 252]; ...
    'c', 231; ...
    'n', 241; ...
    'y', [253, 255] };
  codes = double(out);
  for k = 1:size(groups, 1)
    out(ismember(codes, groups{k, 2})) = groups{k, 1};
  end
end
%=============================================================================
