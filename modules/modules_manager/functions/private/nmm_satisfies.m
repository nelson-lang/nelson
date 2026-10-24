%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = nmm_satisfies(varargin)
  if length(varargin) ~= 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  version = varargin{1};
  constraint = varargin{2};
  if ~ischar(version)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~ischar(constraint)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if isVersionConstraint(constraint)
    tf = semverConstraintSatisfied(version, constraint);
  else
    tf = semver(version, constraint) == 0;
  end
end
%=============================================================================
function tf = semverConstraintSatisfied(version, constraint)
  tf = true;
  constraints = strsplit(strtrim(constraint), ' ');
  for k = 1:length(constraints)
    item = constraints{k};
    if isempty(item)
      continue
    end
    try
      if semver(version, item) ~= 1
        tf = false;
        return
      end
    catch
      tf = false;
      return
    end
  end
end
%=============================================================================
function tf = isVersionConstraint(value)
  tf = startsWith(value, '>') || startsWith(value, '<') ...
      || startsWith(value, '=') || startsWith(value, '^') ...
      || startsWith(value, '~') || contains(value, ' ');
end
%=============================================================================
