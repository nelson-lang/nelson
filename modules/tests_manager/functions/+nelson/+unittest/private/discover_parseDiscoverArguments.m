%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [targets, kind] = discover_parseDiscoverArguments(args) %#ok<NLS0026>
  targets = [];
  kind = 'all';
  if ~isempty(args)
    first = args{1};
    if ~(ischar(first) && startsWith(first, '-'))
      targets = first;
      args(1) = [];
    end
  end
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:tests_manager:optionValueExpected', 'Option value expected.');
    end
    name = args{k};
    value = args{k + 1};
    switch lower(name) %#ok<NLS0008>
      case 'kind'
        kind = value;
      otherwise
        error('Nelson:tests_manager:unsupportedOption', 'Unsupported option.');
    end
    k = k + 2;
  end
end
