%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function options = select_parseSelectOptions(args)
  options = struct('Name', [], 'Module', [], 'File', [], 'Kind', [], ...
    'Tags', [], 'ExcludeTags', [], 'Match', [], 'Exclude', []);
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:tests_manager:optionValueExpected', 'Option value expected.');
    end
    name = args{k};
    value = args{k + 1};
    if isfield(options, name)
      options.(name) = value;
    else
      error('Nelson:tests_manager:unsupportedOption', 'Unsupported option.');
    end
    k = k + 2;
  end
end
