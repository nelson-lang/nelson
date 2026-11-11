%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef calendarDuration
  properties (Dependent)
    Format
    ArithmeticMethod
  end

  properties (Hidden)
    MonthsValue = 0
    DaysValue = 0
    SecondsValue = 0
    FormatValue = 'ymdt'
    % 'default', 'endofmonth' or 'endofquarter' (month or quarter ends kept)
    MethodValue = 'default'
  end

  methods
    function obj = calendarDuration(varargin)
      if nargin == 0
        return
      end
      if nargin == 1 && isa(varargin{1}, 'calendarDuration')
        obj = varargin{1};
        return
      end
      [args, fmt, method, fmtGiven] = calendarDuration.parseInputs(varargin);
      if isempty(args)
        obj.FormatValue = fmt;
        return
      end
      if numel(args) == 1
        value = args{1};
        if isnumeric(value)
          if ismatrix(value) && size(value, 2) == 3
            obj.MonthsValue = double(value(:, 1)) .* 12 + double(value(:, 2));
            obj.DaysValue = double(value(:, 3));
          else
            obj.DaysValue = double(value);
          end
        else
          error('Nelson:time:invalidCalendarDurationInput', 'Invalid %s.', 'calendar duration input');
        end
      elseif numel(args) == 3 || numel(args) == 6
        y = args{1};
        mo = args{2};
        d = args{3};
        if numel(args) == 6
          h = args{4};
          mi = args{5};
          s = args{6};
        else
          h = 0;
          mi = 0;
          s = 0;
        end
        [y, mo, d, h, mi, s] = calendarDuration.expandInputs(y, mo, d, h, mi, s);
        obj.MonthsValue = double(y) .* 12 + double(mo);
        obj.DaysValue = double(d);
        obj.SecondsValue = double(h) .* 3600 + double(mi) .* 60 + double(s);
      else
        error('Nelson:time:invalidCalendarDurationInput', 'Invalid %s.', 'calendar duration input');
      end
      obj.FormatValue = fmt;
      obj = calendarDuration.withMethod(obj, method, fmtGiven);
    end

    function value = get.ArithmeticMethod(obj)
      value = obj.MethodValue;
    end

    function value = get.Format(obj)
      value = obj.FormatValue;
    end

    function obj = set.Format(obj, value)
      obj.FormatValue = calendarDuration.normalizeFormat(value);
    end

    function n = numel(obj, varargin)
      n = numel(calendarDuration.rawMonths(obj), varargin{:});
    end

    function varargout = size(obj, varargin)
      months = calendarDuration.rawMonths(obj);
      if nargout <= 1
        varargout{1} = size(months, varargin{:});
      else
        [varargout{1:nargout}] = size(months, varargin{:});
      end
    end

    function n = length(obj)
      n = length(calendarDuration.rawMonths(obj));
    end

    function n = ndims(obj)
      n = ndims(calendarDuration.rawMonths(obj));
    end

    function tf = isempty(obj)
      tf = isempty(calendarDuration.rawMonths(obj));
    end

    function tf = isscalar(obj)
      tf = isscalar(calendarDuration.rawMonths(obj));
    end

    function tf = isrow(obj)
      tf = isrow(calendarDuration.rawMonths(obj));
    end

    function tf = iscolumn(obj)
      tf = iscolumn(calendarDuration.rawMonths(obj));
    end

    function tf = isvector(obj)
      tf = isvector(calendarDuration.rawMonths(obj));
    end

    function n = end(obj, index, count)
      months = calendarDuration.rawMonths(obj);
      sz = size(months);
      if count == 1
        n = numel(months);
      else
        n = sz(index);
      end
    end

    function varargout = subsref(obj, S)
      if strcmp(S(1).type, '()')
        state = builtin('struct', obj);
        months = state.MonthsValue(S(1).subs{:});
        daysValue = state.DaysValue(S(1).subs{:});
        secondsValue = state.SecondsValue(S(1).subs{:});
        out = calendarDuration.fromParts(months, daysValue, secondsValue, state.FormatValue, ...
          calendarDuration.rawMethod(obj));
        if numel(S) > 1
          varargout{1} = subsref(out, S(2:end));
        else
          varargout{1} = out;
        end
        return
      end
      if strcmp(S(1).type, '.')
        value = calendarDuration.propertyValue(obj, S(1).subs);
        if numel(S) > 1
          value = subsref(value, S(2:end));
        end
        varargout{1} = value;
        return
      end
      error('Nelson:time:invalidCalendarDurationIndexing', 'Invalid %s.', 'calendar duration indexing');
    end

    function obj = subsasgn(obj, S, value)
      if strcmp(S(1).type, '()') && isscalar(S)
        if isnumeric(value) && isempty(value)
          obj.MonthsValue(S(1).subs{:}) = [];
          obj.DaysValue(S(1).subs{:}) = [];
          obj.SecondsValue(S(1).subs{:}) = [];
          return
        end
        if ~isa(value, 'calendarDuration')
          value = calendarDuration(value);
        end
        obj.MonthsValue(S(1).subs{:}) = calendarDuration.rawMonths(value);
        obj.DaysValue(S(1).subs{:}) = calendarDuration.rawDays(value);
        obj.SecondsValue(S(1).subs{:}) = calendarDuration.rawSeconds(value);
        return
      end
      if isscalar(S) && strcmp(S(1).type, '.')
        obj = calendarDuration.assignProperty(obj, S(1).subs, value);
        return
      end
      error('Nelson:time:invalidCalendarDurationAssignment', 'Invalid %s.', 'calendar duration assignment');
    end

    function out = transpose(obj)
      out = calendarDuration.fromParts(calendarDuration.rawMonths(obj).', ...
        calendarDuration.rawDays(obj).', calendarDuration.rawSeconds(obj).', obj.FormatValue, ...
        calendarDuration.rawMethod(obj));
    end

    function out = ctranspose(obj)
      out = transpose(obj);
    end

    function out = reshape(obj, varargin)
      out = calendarDuration.fromParts(reshape(calendarDuration.rawMonths(obj), varargin{:}), ...
        reshape(calendarDuration.rawDays(obj), varargin{:}), ...
        reshape(calendarDuration.rawSeconds(obj), varargin{:}), obj.FormatValue, calendarDuration.rawMethod(obj));
    end

    function out = plus(a, b)
      if isa(a, 'calendarDuration') && isa(b, 'calendarDuration')
        [am, bm] = calendarDuration.expandPair(calendarDuration.rawMonths(a), calendarDuration.rawMonths(b));
        [ad, bd] = calendarDuration.expandPair(calendarDuration.rawDays(a), calendarDuration.rawDays(b));
        [as, bs] = calendarDuration.expandPair(calendarDuration.rawSeconds(a), calendarDuration.rawSeconds(b));
        out = calendarDuration.fromParts(am + bm, ad + bd, as + bs, ...
          calendarDuration.mergeFormats(a.FormatValue, b.FormatValue), calendarDuration.commonMethod({a, b}));
        return
      end
      if isa(a, 'datetime') && isa(b, 'calendarDuration')
        out = plus(a, b);
        return
      end
      if isa(a, 'calendarDuration') && isa(b, 'datetime')
        out = plus(b, a);
        return
      end
      error('Nelson:time:additionDefinedTheseOperands', '%s is not defined for these operands.', 'Addition');
    end

    function out = minus(a, b)
      if isa(a, 'calendarDuration') && isa(b, 'calendarDuration')
        out = a + (-b);
        return
      end
      if isa(a, 'datetime') && isa(b, 'calendarDuration')
        out = minus(a, b);
        return
      end
      error('Nelson:time:subtractionDefinedTheseOperands', '%s is not defined for these operands.', 'Subtraction');
    end

    function out = uminus(obj)
      out = calendarDuration.fromParts(-calendarDuration.rawMonths(obj), ...
        -calendarDuration.rawDays(obj), -calendarDuration.rawSeconds(obj), obj.FormatValue, ...
        calendarDuration.rawMethod(obj));
    end

    function out = times(a, b)
      if isa(a, 'calendarDuration') && isnumeric(b)
        obj = a;
        factor = double(b);
      elseif isa(b, 'calendarDuration') && isnumeric(a)
        obj = b;
        factor = double(a);
      else
        error('Nelson:time:multiplicationDefinedTheseOperands', '%s is not defined for these operands.', ...
          'Multiplication');
      end
      out = calendarDuration.fromParts(calendarDuration.rawMonths(obj) .* factor, ...
        calendarDuration.rawDays(obj) .* factor, calendarDuration.rawSeconds(obj) .* factor, ...
        obj.FormatValue, calendarDuration.rawMethod(obj));
    end

    function out = mtimes(a, b)
      if ~isscalar(a) && ~isscalar(b)
        error('Nelson:time:multiplicationDefinedTheseOperands', '%s is not defined for these operands.', ...
          'Multiplication');
      end
      out = times(a, b);
    end

    function out = rdivide(a, b)
      if ~(isa(a, 'calendarDuration') && isnumeric(b))
        error('Nelson:time:divisionDefinedTheseOperands', '%s is not defined for these operands.', 'Division');
      end
      out = calendarDuration.fromParts(calendarDuration.rawMonths(a) ./ double(b), ...
        calendarDuration.rawDays(a) ./ double(b), calendarDuration.rawSeconds(a) ./ double(b), ...
        a.FormatValue, calendarDuration.rawMethod(a));
    end

    function out = mrdivide(a, b)
      if ~isscalar(a) && ~isscalar(b)
        error('Nelson:time:divisionDefinedTheseOperands', '%s is not defined for these operands.', 'Division');
      end
      out = rdivide(a, b);
    end

    function out = horzcat(varargin)
      out = calendarDuration.concat(2, varargin{:});
    end

    function out = vertcat(varargin)
      out = calendarDuration.concat(1, varargin{:});
    end

    function out = cat(dim, varargin)
      out = calendarDuration.concat(dim, varargin{:});
    end

    function tf = isnan(obj)
      tf = isnan(calendarDuration.rawMonths(obj)) | isnan(calendarDuration.rawDays(obj)) ...
        | isnan(calendarDuration.rawSeconds(obj));
    end

    function tf = isinf(obj)
      tf = isinf(calendarDuration.rawMonths(obj)) | isinf(calendarDuration.rawDays(obj)) ...
        | isinf(calendarDuration.rawSeconds(obj));
    end

    function tf = isfinite(obj)
      tf = isfinite(calendarDuration.rawMonths(obj)) & isfinite(calendarDuration.rawDays(obj)) ...
        & isfinite(calendarDuration.rawSeconds(obj));
    end

    function tf = eq(a, b)
      if ~isa(a, 'calendarDuration') || ~isa(b, 'calendarDuration')
        error('Nelson:time:calendarDurationOperandsExpected', '%s operands expected.', 'Calendar duration');
      end
      [am, bm] = calendarDuration.expandPair(calendarDuration.rawMonths(a), calendarDuration.rawMonths(b));
      [ad, bd] = calendarDuration.expandPair(calendarDuration.rawDays(a), calendarDuration.rawDays(b));
      [as, bs] = calendarDuration.expandPair(calendarDuration.rawSeconds(a), calendarDuration.rawSeconds(b));
      tf = am == bm & ad == bd & as == bs;
      tf(isnan(am) | isnan(bm) | isnan(ad) | isnan(bd) | isnan(as) | isnan(bs)) = false;
    end

    function tf = ne(a, b)
      tf = ~(a == b);
    end

    function tf = isequal(obj, varargin)
      tf = true;
      for k = 1:numel(varargin)
        other = varargin{k};
        if ~isa(other, 'calendarDuration') || ~isequal(size(obj), size(other)) ...
            || ~isequal(calendarDuration.rawMonths(obj), calendarDuration.rawMonths(other)) ...
            || ~isequal(calendarDuration.rawDays(obj), calendarDuration.rawDays(other)) ...
            || ~isequal(calendarDuration.rawSeconds(obj), calendarDuration.rawSeconds(other))
          tf = false;
          return
        end
      end
    end

    function tf = isequaln(obj, varargin)
      tf = true;
      for k = 1:numel(varargin)
        other = varargin{k};
        if ~isa(other, 'calendarDuration') || ~isequal(size(obj), size(other)) ...
            || ~isequaln(calendarDuration.rawMonths(obj), calendarDuration.rawMonths(other)) ...
            || ~isequaln(calendarDuration.rawDays(obj), calendarDuration.rawDays(other)) ...
            || ~isequaln(calendarDuration.rawSeconds(obj), calendarDuration.rawSeconds(other))
          tf = false;
          return
        end
      end
    end

    function out = string(obj)
      % Only non-zero calendar components are shown; when a time component is
      % present it is displayed as hours/minutes/seconds. An all-zero calendar
      % duration is shown as '0d'. Built in one vectorized pass: each of the
      % four possible parts is one compose() over the whole array and the
      % present parts are joined with a single separator, so there is no
      % per-element string accumulation and the cost is linear.
      % years, quarters and weeks are shown only when the Format has y, q or w
      months = calendarDuration.rawMonths(obj);
      daysValue = calendarDuration.rawDays(obj);
      secondsValue = calendarDuration.rawSeconds(obj);
      fmt = obj.FormatValue;
      y = fix(months(:) ./ 12) .* any(fmt == 'y');
      q = fix((months(:) - y .* 12) ./ 3) .* any(fmt == 'q');
      mo = months(:) - y .* 12 - q .* 3;
      w = fix(daysValue(:) ./ 7) .* any(fmt == 'w');
      d = daysValue(:) - w .* 7;
      s = secondsValue(:);
      h = fix(s ./ 3600);
      mi = fix((s - h .* 3600) ./ 60);
      sec = s - h .* 3600 - mi .* 60;
      masks = {y ~= 0, q ~= 0, mo ~= 0, w ~= 0, d ~= 0, s ~= 0};
      texts = {compose('%gy', y), compose('%gq', q), compose('%gmo', mo), ...
        compose('%gw', w), compose('%gd', d), compose('%gh %gm %gs', [h, mi, sec])};
      values = strings(numel(y), 1);
      for p = 1:numel(masks)
        mask = masks{p};
        part = texts{p};
        needSeparator = mask & (strlength(values) > 0);
        values(needSeparator) = values(needSeparator) + " ";
        values(mask) = values(mask) + part(mask);
      end
      % zero is shown in the unit of a month or quarter end method
      zeroTexts = struct('default', "0d", 'endofmonth', "0mo", 'endofquarter', "0q");
      values(strlength(values) == 0) = zeroTexts.(calendarDuration.rawMethod(obj));
      values(isnan(months(:)) | isnan(daysValue(:)) | isnan(s)) = "NaN";
      out = reshape(values, size(months));
    end

    function out = char(obj)
      out = char(string(obj));
    end

    function varargout = split(obj, unit)
      if nargin < 2
        unit = 'ymd';
      end
      if iscell(unit) || isstring(unit)
        varargout = calendarDuration.splitUnits(obj, unit);
        return
      end
      unit = lower(char(unit));
      months = calendarDuration.rawMonths(obj);
      daysValue = calendarDuration.rawDays(obj);
      secondsValue = calendarDuration.rawSeconds(obj);
      switch unit
        case {'years', 'year', 'y'}
          varargout{1} = fix(months ./ 12);
        case {'quarters', 'quarter', 'q'}
          varargout{1} = fix(months ./ 3);
        case {'months', 'month', 'm'}
          varargout{1} = months;
        case {'weeks', 'week', 'w'}
          varargout{1} = daysValue ./ 7;
        case {'days', 'day', 'd'}
          varargout{1} = daysValue;
        case {'time'}
          varargout{1} = seconds(secondsValue);
        otherwise
          y = fix(months ./ 12);
          mo = months - y .* 12;
          if nargout <= 1
            varargout{1} = {y, mo, daysValue, seconds(secondsValue)};
          else
            varargout{1} = y;
            varargout{2} = mo;
            varargout{3} = daysValue;
            varargout{4} = seconds(secondsValue);
          end
      end
    end

    function d = time(obj)
      d = seconds(calendarDuration.rawSeconds(obj));
    end

    function disp(obj)
      nelson.time.displayTextArray(string(obj), 'calendarDuration', false);
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      % a month or quarter end method is named in the header, without the size
      method = calendarDuration.rawMethod(obj);
      header = '';
      if ~strcmp(method, 'default')
        header = sprintf('calendarDuration (ArithmeticMethod="%s")', method);
      end
      nelson.time.displayTextArray(string(obj), 'calendarDuration', false, name, header);
    end
  end

  methods (Hidden)
    texts = compactElementText(obj)
  end

  methods (Static, Hidden)
    function parts = splitUnits(obj, units)
      % one output per unit, larger units taken first from what remains
      units = lower(cellstr(units));
      names = {'years', 'quarters', 'months', 'weeks', 'days', 'time'};
      ranks = zeros(1, numel(units));
      for k = 1:numel(units)
        r = find(strcmp(units{k}, names) | strcmp([units{k}, 's'], names));
        if isempty(r)
          error('Nelson:time:invalidCalendarDurationSplitUnit', 'Invalid %s.', 'calendar duration unit');
        end
        ranks(k) = r;
      end
      months = calendarDuration.rawMonths(obj);
      daysValue = calendarDuration.rawDays(obj);
      values = cell(1, numel(names));
      if any(ranks == 1)
        values{1} = fix(months ./ 12);
        months = months - values{1} .* 12;
      end
      if any(ranks == 2)
        values{2} = fix(months ./ 3);
        months = months - values{2} .* 3;
      end
      values{3} = months;
      if any(ranks == 4)
        values{4} = fix(daysValue ./ 7);
        daysValue = daysValue - values{4} .* 7;
      end
      values{5} = daysValue;
      values{6} = seconds(calendarDuration.rawSeconds(obj));
      parts = values(ranks);
    end

    function obj = empty(varargin)
      z = double.empty(varargin{:});
      obj = calendarDuration.fromParts(z, z, z);
    end

    function obj = fromParts(months, daysValue, secondsValue, formatValue, method)
      obj = calendarDuration();
      obj.MonthsValue = double(months);
      obj.DaysValue = double(daysValue);
      obj.SecondsValue = double(secondsValue);
      if nargin > 3
        obj.FormatValue = formatValue;
      end
      if nargin > 4
        obj.MethodValue = method;
      end
    end

    function value = propertyValue(obj, name)
      % value of a public or stored property
      state = builtin('struct', obj);
      switch name
        case {'Format', 'FormatValue'}
          value = state.FormatValue;
        case {'MonthsValue', 'DaysValue', 'SecondsValue'}
          value = state.(name);
        case {'ArithmeticMethod', 'MethodValue'}
          value = calendarDuration.rawMethod(obj);
        otherwise
          error('Nelson:time:invalidCalendarDurationProperty', 'Invalid %s.', 'calendar duration property');
      end
    end

    function obj = assignProperty(obj, name, value)
      % only the Format can change after creation
      switch name
        case 'Format'
          obj.Format = value;
        case 'ArithmeticMethod'
          error('Nelson:time:readOnlyArithmeticMethod', ...
            'The ArithmeticMethod property is set when the calendar duration is created.');
        otherwise
          error('Nelson:time:invalidCalendarDurationAssignment', 'Invalid %s.', 'calendar duration assignment');
      end
    end

    function method = rawMethod(obj)
      state = builtin('struct', obj);
      method = 'default';
      if isfield(state, 'MethodValue')
        method = state.MethodValue;
      end
    end

    function method = commonMethod(operands)
      % the method shared by every calendarDuration operand, else 'default'
      methods = cellfun(@calendarDuration.rawMethod, ...
        operands(cellfun(@(v) isa(v, 'calendarDuration'), operands)), 'UniformOutput', false);
      method = 'default';
      if ~isempty(methods) && all(strcmp(methods, methods{1}))
        method = methods{1};
      end
    end

    function method = validateMethod(value)
      % 'default', 'endofmonth' or 'endofquarter' (any case)
      names = {'default', 'endofmonth', 'endofquarter'};
      match = calendarDuration.isTextScalar(value) && any(strcmpi(char(value), names));
      if ~match
        error('Nelson:time:invalidArithmeticMethod', ...
          'The arithmetic method must be ''default'', ''endofmonth'' or ''endofquarter''.');
      end
      method = names{strcmpi(char(value), names)};
    end

    function obj = withMethod(obj, method, fmtGiven)
      % A month or quarter end method needs whole months (multiples of 3 for
      % quarters) without days or time; it shows months or quarters.
      if strcmp(method, 'default')
        return
      end
      months = obj.MonthsValue;
      step = 1 + 2 * strcmp(method, 'endofquarter');
      valid = all(obj.DaysValue(:) == 0) && all(obj.SecondsValue(:) == 0) && all(mod(months(:), step) == 0);
      if ~valid
        error('Nelson:time:arithmeticMethodWholeMonths', ...
          'With the arithmetic method ''%s'', the calendar duration must be whole %s without days or time.', ...
          method, calendarDuration.methodUnit(method));
      end
      obj.MethodValue = method;
      if ~fmtGiven
        obj.FormatValue = 'mdt';
        if step == 3
          obj.FormatValue = 'qmdt';
        end
      end
    end

    function unit = methodUnit(method)
      unit = 'months';
      if strcmp(method, 'endofquarter')
        unit = 'quarters';
      end
    end

    function months = rawMonths(obj)
      state = builtin('struct', obj);
      months = state.MonthsValue;
    end

    function daysValue = rawDays(obj)
      state = builtin('struct', obj);
      daysValue = state.DaysValue;
    end

    function secondsValue = rawSeconds(obj)
      state = builtin('struct', obj);
      secondsValue = state.SecondsValue;
    end

    function out = concat(dim, varargin)
      months = cell(1, nargin - 1);
      daysValue = cell(1, nargin - 1);
      secondsValue = cell(1, nargin - 1);
      % the result shows every unit used by a calendarDuration operand
      fmt = '';
      for k = 1:numel(varargin)
        v = varargin{k};
        if isa(v, 'calendarDuration')
          fmt = calendarDuration.mergeFormats(fmt, v.FormatValue);
        else
          v = calendarDuration(v);
        end
        months{k} = calendarDuration.rawMonths(v);
        daysValue{k} = calendarDuration.rawDays(v);
        secondsValue{k} = calendarDuration.rawSeconds(v);
      end
      out = calendarDuration.fromParts(cat(dim, months{:}), cat(dim, daysValue{:}), ...
        cat(dim, secondsValue{:}), calendarDuration.mergeFormats(fmt, 'mdt'), calendarDuration.commonMethod(varargin));
    end

    function [args, fmt, method, fmtGiven] = parseInputs(values)
      args = {};
      fmt = 'ymdt';
      method = 'default';
      fmtGiven = false;
      k = 1;
      while k <= numel(values)
        name = calendarDuration.optionName(values{k});
        if ~isempty(name)
          if k == numel(values)
            error('Nelson:time:missingValueNameValueArgument', 'Missing value for name-value argument.');
          end
          fmtGiven = fmtGiven || strcmp(name, 'format');
          [fmt, method] = calendarDuration.setOption(name, values{k + 1}, fmt, method);
          k = k + 2;
        else
          args{end + 1} = values{k};
          k = k + 1;
        end
      end
    end

    function name = optionName(value)
      % 'format' or 'arithmeticmethod' when value names an option, else ''
      name = '';
      if calendarDuration.isTextScalar(value) && any(strcmpi(char(value), {'Format', 'ArithmeticMethod'}))
        name = lower(char(value));
      end
    end

    function [fmt, method] = setOption(name, value, fmt, method)
      if strcmp(name, 'format')
        fmt = calendarDuration.normalizeFormat(value);
      else
        method = calendarDuration.validateMethod(value);
      end
    end

    function tf = isTextScalar(value)
      tf = ischar(value) || (isstring(value) && isscalar(value));
    end

    function value = validateTextScalar(value, name)
      if isstring(value)
        if ~isscalar(value)
          error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
        end
        value = char(value);
      end
      if ~ischar(value)
        error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
      end
    end

    function value = normalizeFormat(value)
      % letters from 'yqmwdt' in that order; missing m, d or t are added with a warning
      value = calendarDuration.validateTextScalar(value, 'Format');
      letters = 'yqmwdt';
      [found, pos] = ismember(value, letters);
      if isempty(value) || ~isrow(value) || ~all(found)
        error('Nelson:time:unrecognizedCalendarDurationFormat', ...
          'Format ''%s'' is not supported: use only letters from ''yqmwdt''.', value);
      end
      if numel(unique(pos)) ~= numel(pos)
        error('Nelson:time:duplicateCalendarDurationFormat', 'Format ''%s'' repeats a letter.', value);
      end
      if any(diff(pos) < 0)
        error('Nelson:time:calendarDurationFormatOrder', ...
          'Letters of format ''%s'' are out of order: use ''%s''.', value, letters(sort(pos)));
      end
      if ~all(ismember('mdt', value))
        fixed = calendarDuration.mergeFormats(value, 'mdt');
        warning('Nelson:time:calendarDurationFormatIncomplete', ...
          'Format ''%s'' must contain ''m'', ''d'' and ''t'': ''%s'' is used instead.', value, fixed);
        value = fixed;
      end
    end

    function fmt = mergeFormats(a, b)
      % union of two formats in 'yqmwdt' order
      letters = 'yqmwdt';
      fmt = letters(ismember(letters, [a, b]));
    end

    function [a, b, c, d, e, f] = expandInputs(a, b, c, d, e, f)
      targetSize = calendarDuration.commonSize(a, b, c, d, e, f);
      a = calendarDuration.expandToSize(a, targetSize);
      b = calendarDuration.expandToSize(b, targetSize);
      c = calendarDuration.expandToSize(c, targetSize);
      d = calendarDuration.expandToSize(d, targetSize);
      e = calendarDuration.expandToSize(e, targetSize);
      f = calendarDuration.expandToSize(f, targetSize);
    end

    function targetSize = commonSize(varargin)
      targetSize = [1 1];
      for k = 1:numel(varargin)
        if ~isscalar(varargin{k})
          if isequal(targetSize, [1 1])
            targetSize = size(varargin{k});
          elseif ~isequal(targetSize, size(varargin{k}))
            error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
          end
        end
      end
    end

    function x = expandToSize(x, targetSize)
      if isscalar(x) && ~isequal(targetSize, [1 1])
        x = repmat(x, targetSize);
      elseif ~isequal(size(x), targetSize)
        error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
      end
    end

    function [a, b] = expandPair(a, b)
      if isscalar(a) && ~isscalar(b)
        a = repmat(a, size(b));
      elseif isscalar(b) && ~isscalar(a)
        b = repmat(b, size(a));
      elseif ~isequal(size(a), size(b))
        error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
      end
    end

    function out = addToDatetime(t, c, signValue)
      serial = datenum(t);
      months = calendarDuration.rawMonths(c) .* signValue;
      daysValue = calendarDuration.rawDays(c) .* signValue;
      secondsValue = calendarDuration.rawSeconds(c) .* signValue;
      [serial, months] = calendarDuration.expandPair(serial, months);
      [serial, daysValue] = calendarDuration.expandPair(serial, daysValue);
      [serial, secondsValue] = calendarDuration.expandPair(serial, secondsValue);
      v = datevec(serial(:));
      months = months(:);
      v(:, 2) = v(:, 2) + months;
      y = v(:, 1) + floor((v(:, 2) - 1) ./ 12);
      m = mod(v(:, 2) - 1, 12) + 1;
      d = min(v(:, 3), eomday(y, m));
      shifted = datenum(y, m, d, v(:, 4), v(:, 5), v(:, 6));
      shifted = reshape(shifted, size(serial)) + daysValue + secondsValue ./ 86400;
      out = datetime(shifted, 'ConvertFrom', 'datenum', 'Format', t.Format, 'TimeZone', t.TimeZone);
    end
  end
end
%=============================================================================
