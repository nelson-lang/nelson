%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [dCE2, X] = daugment(varargin)
  narginchk(2, Inf);
  nargoutchk(0, 2);

  dCE = varargin{1};
  mruns = positiveInteger(varargin{2}, sprintf(_('%s must be a positive integer scalar.'), 'MRUNS'));
  if ~isnumeric(dCE) || ndims(dCE) ~= 2 || isempty(dCE)
    error('Nelson:statistics:DCENonemptyNumericMatrix', '%s must be a nonempty numeric matrix.', 'DCE');
  end
  modelspec = 'linear';
  args = varargin(3:end);
  if ~isempty(args) && ~(isTextScalar(args{1}) && isOptionName(args{1}))
    modelspec = args{1};
    args = args(2:end);
  end
  [candArgs, exchArgs] = splitOptions(args);
  [dC, C] = candgen(size(dCE, 2), modelspec, candArgs{:});
  fixed = x2fx(dCE, modelspec);
  r = candexch(C, mruns, 'FixedRows', fixed, exchArgs{:});
  dCE2 = [dCE; dC(r, :)];
  X = x2fx(dCE2, modelspec);
end
%=============================================================================
function [candArgs, exchArgs] = splitOptions(args)
  candArgs = {};
  exchArgs = {};
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    name = optionName(args{k});
    if strcmp(name, 'bounds')
      candArgs = [candArgs args(k:k + 1)];
    else
      exchArgs = [exchArgs args(k:k + 1)];
    end
  end
end
%=============================================================================
function tf = isOptionName(value)
  tf = false;
  try
    optionName(value);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function name = optionName(value)
  if ~isTextScalar(value)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'bounds', 'avoidduplicates', 'display', 'initialdesign', 'maxiterations', 'options', 'fixedrows', 'numtries'};
  matches = {};
  for k = 1:length(names)
    if strncmp(names{k}, text, length(text))
      matches{end + 1} = names{k};
    end
  end
  if isempty(matches)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~isscalar(value) || value <= 0 || floor(value) ~= value
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
