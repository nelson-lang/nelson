%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function material(varargin)
  nargoutchk(0, 0);
  inputArguments = convertStringToCharArgs(varargin);
  if isempty(inputArguments)
    inputArguments = {'default'};
  end
  % The first argument may be an explicit target: an axes (apply to its
  % surface/patch children) or one or more surface/patch handles (apply
  % directly), e.g. material(surfaceArray, [..]). Anything else (a name or a
  % numeric vector) applies to the current axes.
  if all(isgraphics(inputArguments{1}(:)))
    target = inputArguments{1};
    inputArguments = inputArguments(2:end);
    if isempty(inputArguments)
      inputArguments = {'default'};
    end
    if isscalar(target) && isgraphics(target, 'axes')
      h = surfacePatchChildren(target);
    else
      h = target;
    end
  else
    h = surfacePatchChildren(gca());
  end
  values = materialValues(inputArguments);
  if ~isempty(h)
    props = {'AmbientStrength', values(1), 'DiffuseStrength', values(2), ...
      'SpecularStrength', values(3)};
    if numel(values) >= 4
      props = [props, {'SpecularExponent', values(4)}];
    end
    if numel(values) >= 5
      props = [props, {'SpecularColorReflectance', values(5)}];
    end
    set(h, props{:});
  end
end
%=============================================================================
function values = materialValues(args)
  if length(args) == 1 && ischar(args{1})
    name = lower(args{1});
    if strcmp(name, 'default')
      values = [0.3, 0.6, 0.9, 10, 1];
    elseif strcmp(name, 'shiny')
      values = [0.3, 0.6, 0.9, 20, 1];
    elseif strcmp(name, 'dull')
      values = [0.3, 0.8, 0.0, 10, 1];
    elseif strcmp(name, 'metal')
      values = [0.3, 0.3, 1.0, 25, 0.5];
    else
      error('Nelson:material:invalidPropertyValue', 'Illegal selection for property.');
    end
  elseif length(args) == 1 && isnumeric(args{1}) && isvector(args{1})
    values = args{1}(:)';
  else
    values = cell2mat(args);
  end
  if ~isnumeric(values) || ~isvector(values) || ~any(numel(values) == [3 4 5])
    error('Nelson:material:invalidMaterialValues', 'Material values must be a three, four or five element vector.');
  end
end
%=============================================================================
