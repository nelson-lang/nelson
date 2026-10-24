%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function box(varargin)
  narginchk(0, 2);
  nargoutchk(0, 0);

  if nargin == 0
    target = gca();
    toggleBox(target);
    return
  end

  firstArgument = varargin{1};
  if isBoxTargetArgument(firstArgument)
    target = firstArgument;
    if nargin == 1
      validateBoxTarget(target);
      toggleBox(target);
    else
      validateBoxTarget(target);
      setBox(target, parseBoxVisibility(varargin{2}));
    end
    return
  end

  if nargin == 2
    error('Nelson:box:invalidFirstArgument', 'First argument must be a graphics object.');
  end

  target = gca();
  validateBoxTarget(target);
  setBox(target, parseBoxVisibility(firstArgument));
end
%=============================================================================
function tf = isBoxTargetArgument(value)
  if isempty(value)
    tf = false;
    return
  else
    graphicsFlags = isgraphics(value);
    tf = all(graphicsFlags(:));
  end
end
%=============================================================================
function validateBoxTarget(target)
  for k = 1:numel(target)
    if ~isprop(target(k), 'Box')
      error('Nelson:box:invalidTarget', 'Target must have a Box property.');
    end
  end
end
%=============================================================================
function toggleBox(target)
  for k = 1:numel(target)
    if strcmp(target(k).Box, 'on')
      target(k).Box = 'off';
    else
      target(k).Box = 'on';
    end
  end
end
%=============================================================================
function setBox(target, visibility)
  for k = 1:numel(target)
    if ~strcmp(target(k).Box, visibility)
      target(k).Box = visibility;
    end
  end
end
%=============================================================================
function visibility = parseBoxVisibility(value)
  if ischar(value) || isStringScalar(value)
    visibility = lower(convertStringsToChars(value));
    if strcmp(visibility, 'on') || strcmp(visibility, 'off')
      return
    end
  elseif (islogical(value) || isnumeric(value)) && isscalar(value)
    if value == 1
      visibility = 'on';
      return
    elseif value == 0
      visibility = 'off';
      return
    end
  end
  error('Nelson:box:invalidOnOffValue', 'Command option must be ''on'' or ''off''.');
end
%=============================================================================
