%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = synchronize(varargin)
  narginchk(1, Inf);
  parsed = parseSynchronizeInputsLocal(varargin{:});
  inputs = parsed.Inputs;
  method = parsed.Method;
  newTimes = parsed.NewTimes;
  [~, order] = sort(newTimes);
  newTimes = newTimes(order);
  TT = retime(inputs{1}, newTimes, method, parsed.Options{:});
  for k = 2:length(inputs)
    next = retime(inputs{k}, newTimes, method, parsed.Options{:});
    next = renameDuplicatesLocal(TT, next, k);
    TT = [TT, next];
  end
end
%=============================================================================
function parsed = parseSynchronizeInputsLocal(varargin)
  parsed = struct('Inputs', {{}}, 'NewTimes', [], 'Method', 'fillwithmissing', 'Options', {{}});
  firstExtra = 1;
  while firstExtra <= nargin && istimetable(varargin{firstExtra})
    parsed.Inputs{end + 1} = varargin{firstExtra};
    firstExtra = firstExtra + 1;
  end
  if isempty(parsed.Inputs)
    error('Nelson:timetable:allInputsMustBe', 'All inputs must be %s.', 'timetables');
  end
  args = varargin(firstExtra:end);
  if isempty(args)
    parsed.NewTimes = synchronizeTimesLocal(parsed.Inputs, 'union');
    return
  end
  basis = args{1};
  args(1) = [];
  if isTextScalarLocal(basis)
    basisText = lower(char(basis));
    if any(strcmp(basisText, {'union', 'intersection', 'first', 'last'}))
      parsed.NewTimes = synchronizeTimesLocal(parsed.Inputs, basisText);
    elseif strcmp(basisText, 'regular')
      [parsed.Method, args] = consumeMethodLocal(args, parsed.Method);
      [step, args] = consumeRegularStepLocal(args);
      parsed.NewTimes = regularTimesLocal(parsed.Inputs, step);
    elseif ~isempty(namedStepLocal(basisText))
      parsed.NewTimes = regularTimesLocal(parsed.Inputs, namedStepLocal(basisText));
    elseif isMethodNameLocal(basisText)
      parsed.NewTimes = synchronizeTimesLocal(parsed.Inputs, 'union');
      parsed.Method = basisText;
    else
      parsed.NewTimes = columnTimesLocal(basis);
    end
  else
    parsed.NewTimes = columnTimesLocal(basis);
  end
  [parsed.Method, args] = consumeMethodLocal(args, parsed.Method);
  parsed.Options = args;
end
%=============================================================================
function times = synchronizeTimesLocal(inputs, how)
  switch how
    case 'first'
      times = inputs{1}.Properties.RowTimes;
    case 'last'
      times = inputs{end}.Properties.RowTimes;
    otherwise
      times = inputs{1}.Properties.RowTimes;
      for k = 2:length(inputs)
        if strcmp(how, 'intersection')
          times = intersectTimesLocal(times, inputs{k}.Properties.RowTimes);
        else
          times = uniqueTimesLocal([times; inputs{k}.Properties.RowTimes]);
        end
      end
  end
end
%=============================================================================
function times = regularTimesLocal(inputs, step)
  first = inputs{1}.Properties.RowTimes;
  lo = min(first);
  hi = max(first);
  for k = 2:length(inputs)
    tk = inputs{k}.Properties.RowTimes;
    lo = min([lo; min(tk)]);
    hi = max([hi; max(tk)]);
  end
  times = (floorTimeLocal(lo, step):step:hi)';
end
%=============================================================================
function [method, args] = consumeMethodLocal(args, defaultMethod)
  method = defaultMethod;
  if ~isempty(args) && isMethodNameLocal(args{1})
    method = lower(char(args{1}));
    args(1) = [];
  end
end
%=============================================================================
function [step, args] = consumeRegularStepLocal(args)
  step = [];
  k = 1;
  while k < numel(args)
    name = args{k};
    value = args{k + 1};
    if isTextScalarLocal(name) && strcmpi(char(name), 'TimeStep')
      step = value;
      args(k:k + 1) = [];
      return
    elseif isTextScalarLocal(name) && strcmpi(char(name), 'SampleRate')
      step = seconds(1 / double(value));
      args(k:k + 1) = [];
      return
    end
    k = k + 2;
  end
  error('Nelson:timetable:invalidSyntax:regularSynchronizationRequiresTimeStepSampleRate', 'Regular synchronization requires TimeStep or SampleRate.');
end
%=============================================================================
function tf = isMethodNameLocal(value)
  if ~isTextScalarLocal(value)
    tf = false;
    return
  end
  methods = {'fillwithmissing', 'fillwithconstant', 'nearest', 'previous', 'next', ...
    'linear', 'spline', 'pchip', 'makima', 'sum', 'mean', 'min', 'max', ...
    'median', 'prod', 'count', 'firstvalue', 'lastvalue'};
  tf = any(strcmpi(char(value), methods));
end
%=============================================================================
function step = namedStepLocal(value)
  step = [];
  if ~isTextScalarLocal(value)
    return
  end
  switch lower(char(value))
    case 'yearly'
      step = calyears(1);
    case 'quarterly'
      step = calquarters(1);
    case 'monthly'
      step = calmonths(1);
    case 'weekly'
      step = calweeks(1);
    case 'daily'
      step = caldays(1);
    case 'hourly'
      step = hours(1);
    case 'minutely'
      step = minutes(1);
    case 'secondly'
      step = seconds(1);
  end
end
%=============================================================================
function t = floorTimeLocal(t, step)
  if isa(t, 'datetime') && isa(step, 'duration')
    secondsPerStep = seconds(step);
    serial = floor(datenum(t) .* 86400 ./ secondsPerStep) .* secondsPerStep ./ 86400;
    t = datetime(serial, 'ConvertFrom', 'datenum', 'Format', t.Format, 'TimeZone', t.TimeZone);
  elseif isa(t, 'duration') && isa(step, 'duration')
    t = seconds(floor(seconds(t) ./ seconds(step)) .* seconds(step));
  end
end
%=============================================================================
function out = uniqueTimesLocal(times)
  out = emptyTimesLikeLocal(times);
  for k = 1:timeLengthLocal(times)
    if isempty(find(out == times(k), 1))
      out(end + 1, 1) = times(k);
    end
  end
end
%=============================================================================
function value = columnTimesLocal(value)
  if isa(value, 'datetime') || isa(value, 'duration')
    value = value(:);
  elseif isrow(value)
    value = value(:);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
function out = intersectTimesLocal(a, b)
  out = emptyTimesLikeLocal(a);
  for k = 1:timeLengthLocal(a)
    if ~isempty(find(b == a(k), 1)) && isempty(find(out == a(k), 1))
      out(end + 1, 1) = a(k);
    end
  end
end
%=============================================================================
function n = timeLengthLocal(value)
  if isa(value, 'datetime')
    n = numel(datenum(value));
  elseif isa(value, 'duration')
    n = numel(seconds(value));
  else
    n = length(value);
  end
end
%=============================================================================
function out = emptyTimesLikeLocal(value)
  if isa(value, 'datetime')
    out = datetime([], [], []);
    out = out(:);
  elseif isa(value, 'duration')
    out = seconds(zeros(0, 1));
  else
    out = value([]);
  end
end
%=============================================================================
function TT = renameDuplicatesLocal(existing, TT, suffix)
  existingNames = existing.Properties.VariableNames;
  names = TT.Properties.VariableNames;
  for k = 1:length(names)
    if any(strcmp(existingNames, names{k}))
      names{k} = [names{k}, '_', int2str(suffix)];
    end
  end
  st = struct(TT);
  T = st.Table;
  T.Properties.VariableNames = names;
  TT = timetable.fromTableAndTimes(T, st.Properties.RowTimes, st.Properties);
end
%=============================================================================
