%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [r, msg] = nmm_is_supported_platform(platforms, currentArch)
  % currentArch defaults to the running architecture; nmm('pack', ...,
  % '-target', 'wasm32') checks the package against the target instead.
  r = false;
  msg = 'Invalid type in platforms';
  validPlatforms = nmm_valid_platforms();
  if nargin < 2
    currentArch = computer('arch');
  end

  if ischar(platforms)
    platforms = {platforms};
  elseif isstring(platforms)
    platforms = cellstr(platforms);
  elseif ~iscellstr(platforms)
    return
  end

  for platform = platforms(:)'
    if ~any(strcmp(validPlatforms, platform{1}))
      msg = sprintf('Platform not supported: %s', platform{1});
      return
    end
  end

  if any(strcmp(platforms, 'all')) || any(strcmp(platforms, currentArch))
    r = true;
    msg = '';
  else
    if length(platforms) == 1
      msg = sprintf('Platform not supported: %s', platforms{1});
    else
      msg = sprintf('Platform not supported: %s', sprintf('%s ', platforms{:}));
    end
  end
end
%=============================================================================
