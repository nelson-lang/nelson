%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function response = ai_mcp_response_by_id(output, id)
  lines = split_mcp_lines(output);
  response = [];
  for k = 1:numel(lines)
    line = lines{k};
    if isempty(line)
      continue;
    end
    decoded = jsondecode(line);
    if isstruct(decoded) && isfield(decoded, 'id')
      for j = 1:numel(decoded)
        if isequal(decoded(j).id, id)
          response = decoded(j);
          return;
        end
      end
    elseif iscell(decoded)
      for j = 1:numel(decoded)
        item = decoded{j};
        if isstruct(item) && isfield(item, 'id') && isequal(item.id, id)
          response = item;
          return;
        end
      end
    end
  end
end
%=============================================================================
function lines = split_mcp_lines(output)
  lines = {};
  starts = [1, strfind(output, newline()) + 1];
  stops = [strfind(output, newline()) - 1, length(output)];
  for k = 1:numel(starts)
    if stops(k) < starts(k)
      lines{end + 1} = '';
    else
      line = output(starts(k):stops(k));
      if ~isempty(line) && line(end) == char(13)
        line(end) = [];
      end
      lines{end + 1} = line;
    end
  end
end
%=============================================================================
