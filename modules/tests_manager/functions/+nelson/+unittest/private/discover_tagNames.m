%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tags = discover_tagNames(options)
  names = fieldnames(options);
  tags = {};
  for k = 1:length(names)
    if ~strcmp(names{k}, 'weight') && ~strcmp(names{k}, 'timeout') && options.(names{k})
      tags = [tags; names{k}];
    end
  end
end
