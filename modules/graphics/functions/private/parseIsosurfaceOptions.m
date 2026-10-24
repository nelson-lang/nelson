%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [args, noshare, verbose] = parseIsosurfaceOptions(args)
  noshare = false;
  verbose = false;
  optionIndex = 0;
  while optionIndex < 2
    optionIndex = optionIndex + 1;
    if isempty(args) || ~ischar(args{end})
      return
    end
    [isNoshare, isVerbose] = parseOptionValue(args{end});
    noshare = noshare || isNoshare;
    verbose = verbose || isVerbose;
    args(end) = [];
  end
end
%=============================================================================
function [noshare, verbose] = parseOptionValue(option)
  noshare = false;
  verbose = false;
  switch lower(option)
    case 'noshare'
      noshare = true;
    case 'verbose'
      verbose = true;
    otherwise
      error('Nelson:parseIsosurfaceOptions:invalidIsosurfaceOption', 'Invalid isosurface option.');
  end
end
%=============================================================================
