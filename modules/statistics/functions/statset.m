%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = statset(varargin)
  if nargin == 0
    options = defaultOptions('');
    if nargout == 0
      displayOptionsHelp();
      clear options;
    end
    return
  end

  if isTextScalar(varargin{1}) && (nargin == 1 || ~isOptionName(varargin{1}))
    options = defaultOptions(char(varargin{1}));
    args = varargin(2:end);
  elseif isstruct(varargin{1})
    options = normalizeOptions(varargin{1});
    args = varargin(2:end);
  else
    options = defaultOptions('');
    args = varargin;
  end

  if ~isempty(args) && isstruct(args{1})
    options = mergeOptions(options, normalizeOptions(args{1}));
    args = args(2:end);
  end

  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  options = applyNameValues(options, args);
end
%=============================================================================
function tf = isOptionName(value)
  tf = false;
  try
    canonicalField(char(value));
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function options = defaultOptions(statfun)
  options = emptyOptions();
  name = lower(statfun);
  if strcmp(name, 'kmeans') || strcmp(name, 'kmedoids')
    options.Display = 'off';
    options.MaxIter = 100;
    options.UseParallel = false;
    options.UseSubstreams = false;
    options.Streams = {};
  elseif strcmp(name, 'fitgmdist')
    options.Display = 'off';
    options.MaxIter = 100;
    options.TolFun = 1e-6;
  elseif strcmp(name, 'bootci') || strcmp(name, 'bootstrp') || strcmp(name, 'candexch')
    options.UseParallel = false;
    options.UseSubstreams = false;
    options.Streams = {};
  elseif strcmp(name, 'parallel')
    options.UseParallel = false;
    options.UseSubstreams = false;
    options.Streams = {};
  end
end
%=============================================================================
function options = emptyOptions()
  options = struct('Display', [], ...
    'MaxFunEvals', [], ...
    'MaxIter', [], ...
    'TolBnd', [], ...
    'TolFun', [], ...
    'TolTypeFun', [], ...
    'TolX', [], ...
    'TolTypeX', [], ...
    'GradObj', [], ...
    'Jacobian', [], ...
    'DerivStep', [], ...
    'FunValCheck', [], ...
    'Robust', [], ...
    'RobustWgtFun', [], ...
    'WgtFun', [], ...
    'Tune', [], ...
    'UseParallel', [], ...
    'UseSubstreams', [], ...
    'Streams', {{}}, ...
    'OutputFcn', []);
end
%=============================================================================
function options = normalizeOptions(inputOptions)
  options = emptyOptions();
  names = fieldnames(inputOptions);
  for k = 1:length(names)
    canonical = canonicalField(names{k});
    options.(canonical) = inputOptions.(names{k});
  end
end
%=============================================================================
function options = mergeOptions(oldOptions, newOptions)
  options = newOptions;
  names = fieldnames(oldOptions);
  for k = 1:length(names)
    name = names{k};
    if isempty(options.(name))
      options.(name) = oldOptions.(name);
    end
  end
end
%=============================================================================
function options = applyNameValues(options, args)
  for k = 1:2:length(args)
    if ~isTextScalar(args{k})
      error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
    field = canonicalField(char(args{k}));
    options.(field) = args{k + 1};
  end
end
%=============================================================================
function field = canonicalField(name)
  fields = fieldnames(emptyOptions());
  lowerName = lower(name);
  matches = {};
  for k = 1:length(fields)
    candidate = lower(fields{k});
    if strcmp(candidate, lowerName)
      field = fields{k};
      return
    end
    if strncmp(candidate, lowerName, length(lowerName))
      matches{end + 1} = fields{k};
    end
  end
  if isempty(matches)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  field = matches{1};
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function displayOptionsHelp()
  disp('                Display: [ {off} | final | iter ]');
  disp('            MaxFunEvals: [ positive integer ]');
  disp('                MaxIter: [ positive integer ]');
  disp('                 TolBnd: [ positive scalar ]');
  disp('                 TolFun: [ positive scalar ]');
  disp('             TolTypeFun: [ abs | rel ]');
  disp('                   TolX: [ positive scalar ]');
  disp('               TolTypeX: [ abs | rel ]');
  disp('                GradObj: [ off | on ]');
  disp('               Jacobian: [ off | on ]');
  disp('              DerivStep: [ positive scalar or vector ]');
  disp('            FunValCheck: [ off | on ]');
  disp('           RobustWgtFun: [ [] | bisquare | andrews | cauchy | fair | huber | logistic | talwar | welsch | function handle ]');
  disp('                   Tune: [ positive scalar ]');
  disp('            UseParallel: [ false | true | on | off | auto ]');
  disp('          UseSubstreams: [ false | true ]');
  disp('                Streams: [ {} | stream or cell array ]');
  disp('              OutputFcn: [ [] | function handle or cell array ]');
end
%=============================================================================
