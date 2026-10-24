%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function state = tuneWeights_fileState(filename)
  if ~isfile(filename)
    error('Nelson:tests_manager:fileDoesNotExist', 'An existing filename expected.');
  end
  options = parsetags(filename); %#ok<NLS0004>
  lines = tuneWeights_sourceLines(fileread(filename));
  state = struct('has_tag', tuneWeights_tagIndex(lines) > 0, ...
    'weight', double(options.weight));
end
%=============================================================================
function lines = tuneWeights_sourceLines(content)
  sourceText = replace(content, char([13 10]), char(10));
  sourceText = replace(sourceText, char(13), char(10));
  lines = strsplit(sourceText, char(10), 'CollapseDelimiters', false);
end
%=============================================================================
function index = tuneWeights_tagIndex(lines)
  index = 0;
  pattern = '^[ \t]*%[ \t]*<--WEIGHT[ \t]+[0-9]+-->[ \t]*$';
  for k = 1:length(lines)
    line = strtrim(lines{k});
    if ~isempty(line) && ~startsWith(line, '%')
      return
    end
    if ~isempty(regexp(lines{k}, pattern, 'once'))
      index = k;
      return
    end
  end
end
%=============================================================================
