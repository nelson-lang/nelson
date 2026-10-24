%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef duration
  properties (Dependent)
    Format
  end

  properties (Hidden)
    SecondsValue = 0
    FormatValue = 'hh:mm:ss'
  end

  methods
    function obj = duration(varargin)
      obj.SecondsValue = 0;
      obj.FormatValue = 'hh:mm:ss';
      if nargin == 0
        return
      end
      if nargin == 1 && isa(varargin{1}, 'duration')
        obj = varargin{1};
        return
      end

      [args, inputFormat, outputFormat] = duration.parseNameValue(varargin);
      if isempty(args)
        error('Nelson:time:invalidDurationInputArguments', 'Invalid %s.', 'duration input arguments');
      end

      if numel(args) == 1
        value = args{1};
        if isa(value, 'duration')
          obj = value;
        elseif isnumeric(value)
          obj.SecondsValue = duration.secondsFromNumericMatrix(value);
        elseif isa(value, 'missing')
          obj.SecondsValue = NaN(size(value));
        elseif ischar(value) || isstring(value) || iscellstr(value)
          obj.SecondsValue = duration.parseTextArray(value, inputFormat);
        else
          error('Nelson:time:invalidDurationInputArgument', 'Invalid %s.', 'duration input argument');
        end
      elseif numel(args) == 3 || numel(args) == 4
        h = args{1};
        mi = args{2};
        s = args{3};
        ms = 0;
        if numel(args) == 4
          ms = args{4};
        end
        [h, mi, s, ms] = duration.expandInputs(h, mi, s, ms);
        obj.SecondsValue = double(h) .* 3600 + double(mi) .* 60 + double(s) + double(ms) ./ 1000;
      else
        error('Nelson:time:invalidDurationInputArguments', 'Invalid %s.', 'duration input arguments');
      end

      obj.FormatValue = outputFormat;
    end

    function value = get.Format(obj)
      value = obj.FormatValue;
    end

    function obj = set.Format(obj, value)
      obj.FormatValue = duration.validateTextScalar(value, 'Format');
    end

    function varargout = subsref(obj, S)
      if strcmp(S(1).type, '()')
        values = double(obj);
        out = duration.fromSeconds(values(S(1).subs{:}), obj.FormatValue);
        if numel(S) > 1
          varargout{1} = subsref(out, S(2:end));
        else
          varargout{1} = out;
        end
        return
      end
      if strcmp(S(1).type, '.') && strcmp(S(1).subs, 'Format')
        value = obj.FormatValue;
        if numel(S) > 1
          value = subsref(value, S(2:end));
        end
        varargout{1} = value;
        return
      end
      if strcmp(S(1).type, '.') && strcmp(S(1).subs, 'SecondsValue')
        state = builtin('struct', obj);
        value = state.SecondsValue;
        if numel(S) > 1
          value = subsref(value, S(2:end));
        end
        varargout{1} = value;
        return
      end
      if strcmp(S(1).type, '.') && strcmp(S(1).subs, 'FormatValue')
        state = builtin('struct', obj);
        value = state.FormatValue;
        if numel(S) > 1
          value = subsref(value, S(2:end));
        end
        varargout{1} = value;
        return
      end
      error('Nelson:time:invalidDurationIndexing', 'Invalid %s.', 'duration indexing');
    end

    function obj = subsasgn(obj, S, value)
      if strcmp(S(1).type, '()')
        if ~isa(value, 'duration')
          value = duration(value);
        end
        obj.SecondsValue(S(1).subs{:}) = value.SecondsValue;
        return
      end
      if strcmp(S(1).type, '.') && strcmp(S(1).subs, 'Format')
        obj.Format = value;
        return
      end
      error('Nelson:time:invalidDurationAssignment', 'Invalid %s.', 'duration assignment');
    end

    function out = horzcat(varargin)
      out = duration.concat(2, varargin{:});
    end

    function out = vertcat(varargin)
      out = duration.concat(1, varargin{:});
    end

    function out = cat(dim, varargin)
      out = duration.concat(dim, varargin{:});
    end

    function out = transpose(obj)
      out = duration.fromSeconds(obj.SecondsValue.', obj.FormatValue);
    end

    function out = ctranspose(obj)
      out = transpose(obj);
    end

    function out = reshape(obj, varargin)
      out = duration.fromSeconds(reshape(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function tf = isnan(obj)
      tf = isnan(double(obj));
    end

    function tf = isinf(obj)
      tf = isinf(double(obj));
    end

    function tf = isfinite(obj)
      tf = isfinite(double(obj));
    end

    function [out, index] = sort(obj, varargin)
      [values, index] = sort(double(obj), varargin{:});
      out = duration.fromSeconds(values, obj.FormatValue);
    end

    function [C, ia, ic] = unique(obj, varargin)
      % Set operations delegate to the seconds representation, mirroring
      % sort/min/max, then rebuild the result as a duration with the same
      % Format. Each NaN duration is kept distinct, matching the numeric result.
      secondsValue = double(obj);
      [values, ia, ic] = unique(secondsValue, varargin{:});
      C = duration.fromSeconds(values, obj.FormatValue);
    end

    function [tf, loc] = ismember(a, b, varargin)
      leftSeconds = double(a);
      rightSeconds = double(b);
      [tf, loc] = ismember(leftSeconds, rightSeconds, varargin{:});
    end

    function [C, ia, ib] = intersect(a, b, varargin)
      leftSeconds = double(a);
      rightSeconds = double(b);
      [values, ia, ib] = intersect(leftSeconds, rightSeconds, varargin{:});
      C = duration.fromSeconds(values, duration.formatOf(a, b));
    end

    function [C, ia, ib] = union(a, b, varargin)
      leftSeconds = double(a);
      rightSeconds = double(b);
      [values, ia, ib] = union(leftSeconds, rightSeconds, varargin{:});
      C = duration.fromSeconds(values, duration.formatOf(a, b));
    end

    function [C, ia] = setdiff(a, b, varargin)
      leftSeconds = double(a);
      rightSeconds = double(b);
      [values, ia] = setdiff(leftSeconds, rightSeconds, varargin{:});
      C = duration.fromSeconds(values, duration.formatOf(a, b));
    end

    function tf = isequal(obj, varargin)
      tf = true;
      for k = 1:numel(varargin)
        other = varargin{k};
        if ~isa(other, 'duration') || ~isequal(size(obj), size(other)) ...
            || ~isequal(double(obj), double(other))
          tf = false;
          return
        end
      end
    end

    function tf = isequalto(obj, other)
      tf = isequal(obj, other);
    end

    function tf = isequaln(obj, varargin)
      tf = true;
      for k = 1:numel(varargin)
        other = varargin{k};
        if ~isa(other, 'duration') || ~isequal(size(obj), size(other)) ...
            || ~isequaln(double(obj), double(other))
          tf = false;
          return
        end
      end
    end

    function out = uplus(obj)
      out = obj;
    end

    function out = uminus(obj)
      out = duration.fromSeconds(-obj.SecondsValue, obj.FormatValue);
    end

    function out = abs(obj)
      out = duration.fromSeconds(abs(obj.SecondsValue), obj.FormatValue);
    end

    function out = sum(obj, varargin)
      out = duration.fromSeconds(sum(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function out = mean(obj, varargin)
      out = duration.fromSeconds(mean(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function out = median(obj, varargin)
      out = duration.fromSeconds(median(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function out = mode(obj, varargin)
      out = duration.fromSeconds(mode(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function out = std(obj, varargin)
      out = duration.fromSeconds(std(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function out = cumsum(obj, varargin)
      out = duration.fromSeconds(cumsum(obj.SecondsValue, varargin{:}), obj.FormatValue);
    end

    function out = floor(obj)
      out = duration.fromSeconds(floor(obj.SecondsValue), obj.FormatValue);
    end

    function out = ceil(obj)
      out = duration.fromSeconds(ceil(obj.SecondsValue), obj.FormatValue);
    end

    function out = round(obj, unit)
      if nargin < 2
        out = duration.fromSeconds(round(obj.SecondsValue), obj.FormatValue);
        return
      end
      unitSeconds = duration.unitInSeconds(unit);
      out = duration.fromSeconds(round(obj.SecondsValue ./ unitSeconds) .* unitSeconds, ...
        obj.FormatValue);
    end

    function out = mod(a, b)
      if ~isa(a, 'duration') || ~isa(b, 'duration')
        error('Nelson:time:modDurationRequiresBothArguments', '%s of a duration requires both arguments to be durations.', 'mod');
      end
      [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
      out = duration.fromSeconds(mod(av, bv), a.FormatValue);
    end

    function out = rem(a, b)
      if ~isa(a, 'duration') || ~isa(b, 'duration')
        error('Nelson:time:remDurationRequiresBothArguments', '%s of a duration requires both arguments to be durations.', 'rem');
      end
      [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
      out = duration.fromSeconds(rem(av, bv), a.FormatValue);
    end

    function [out, idx] = min(a, b, varargin)
      idx = [];
      if nargin >= 2 && isa(b, 'duration')
        [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
        out = duration.fromSeconds(min(av, bv), a.FormatValue);
      elseif nargin == 1
        [v, idx] = min(a.SecondsValue);
        out = duration.fromSeconds(v, a.FormatValue);
      else
        [v, idx] = min(a.SecondsValue, b, varargin{:});
        out = duration.fromSeconds(v, a.FormatValue);
      end
    end

    function [out, idx] = max(a, b, varargin)
      idx = [];
      if nargin >= 2 && isa(b, 'duration')
        [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
        out = duration.fromSeconds(max(av, bv), a.FormatValue);
      elseif nargin == 1
        [v, idx] = max(a.SecondsValue);
        out = duration.fromSeconds(v, a.FormatValue);
      else
        [v, idx] = max(a.SecondsValue, b, varargin{:});
        out = duration.fromSeconds(v, a.FormatValue);
      end
    end

    function out = plus(a, b)
      if isa(a, 'duration') && isa(b, 'duration')
        [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
        out = duration.fromSeconds(av + bv, a.FormatValue);
        return
      end
      if isa(a, 'datetime') || isa(b, 'datetime')
        out = plusDatetimeDuration(a, b);
        return
      end
      error('Nelson:time:additionDefinedTheseOperands', '%s is not defined for these operands.', 'Addition');
    end

    function out = minus(a, b)
      if isa(a, 'duration') && isa(b, 'duration')
        [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
        out = duration.fromSeconds(av - bv, a.FormatValue);
        return
      end
      error('Nelson:time:subtractionDefinedTheseOperands', '%s is not defined for these operands.', 'Subtraction');
    end

    function out = times(a, b)
      if isa(a, 'duration') && isnumeric(b)
        out = duration.fromSeconds(a.SecondsValue .* double(b), a.FormatValue);
        return
      end
      if isa(b, 'duration') && isnumeric(a)
        out = duration.fromSeconds(double(a) .* b.SecondsValue, b.FormatValue);
        return
      end
      error('Nelson:time:multiplicationDefinedTheseOperands', '%s is not defined for these operands.', 'Multiplication');
    end

    function out = rdivide(a, b)
      if isa(a, 'duration') && isnumeric(b)
        out = duration.fromSeconds(a.SecondsValue ./ double(b), a.FormatValue);
        return
      end
      if isa(a, 'duration') && isa(b, 'duration')
        [av, bv] = duration.expandPair(a.SecondsValue, b.SecondsValue);
        out = av ./ bv;
        return
      end
      error('Nelson:time:divisionDefinedTheseOperands', '%s is not defined for these operands.', 'Division');
    end

    function out = mtimes(a, b)
      % Scalar scaling behaves element-wise; matrix products of durations are
      % not supported unless one operand is scalar.
      if ~isscalar(a) && ~isscalar(b)
        error('Nelson:time:multiplicationDefinedTheseOperands', '%s is not defined for these operands.', 'Multiplication');
      end
      out = times(a, b);
    end

    function out = mrdivide(a, b)
      if ~isscalar(a) && ~isscalar(b)
        error('Nelson:time:divisionDefinedTheseOperands', '%s is not defined for these operands.', 'Division');
      end
      out = rdivide(a, b);
    end

    function out = eq(a, b)
      [av, bv] = duration.compareInputs(a, b);
      out = av == bv;
    end

    function out = ne(a, b)
      out = ~(a == b);
    end

    function out = lt(a, b)
      [av, bv] = duration.compareInputs(a, b);
      out = av < bv;
    end

    function out = le(a, b)
      [av, bv] = duration.compareInputs(a, b);
      out = av <= bv;
    end

    function out = gt(a, b)
      [av, bv] = duration.compareInputs(a, b);
      out = av > bv;
    end

    function out = ge(a, b)
      [av, bv] = duration.compareInputs(a, b);
      out = av >= bv;
    end

    function out = colon(a, b, c)
      if nargin == 2
        first = a;
        step = seconds(1);
        last = b;
      else
        first = a;
        step = b;
        last = c;
      end
      if ~isa(first, 'duration') || ~isa(step, 'duration') || ~isa(last, 'duration')
        error('Nelson:time:durationColonOperandsExpected', '%s colon operands expected.', 'Duration');
      end
      out = duration.fromSeconds(first.SecondsValue:step.SecondsValue:last.SecondsValue, first.FormatValue);
    end

    function value = double(obj)
      value = duration.rawSeconds(obj);
    end

    function varargout = hms(obj)
      total = obj.SecondsValue;
      signValue = sign(total);
      totalAbs = abs(total);
      h = floor(totalAbs ./ 3600);
      r = totalAbs - h .* 3600;
      mi = floor(r ./ 60);
      s = r - mi .* 60;
      if nargout <= 1
        varargout{1} = {h .* signValue, mi, s};
      else
        varargout{1} = h .* signValue;
        varargout{2} = mi;
        varargout{3} = s;
      end
    end

    function out = char(obj, varargin)
      if nargin > 1
        obj.Format = varargin{1};
      end
      out = char(durationFormatText(obj));
    end

    function out = string(obj, varargin)
      if nargin > 1
        obj.Format = varargin{1};
      end
      out = durationFormatText(obj);
      out(isnan(double(obj))) = missing;
    end

    function disp(obj)
      nelson.time.displayTextArray(durationFormatText(obj), 'duration', false);
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      nelson.time.displayTextArray(durationFormatText(obj), 'duration', false, name);
    end
  end

  methods (Hidden)
    texts = compactElementText(obj)
  end

  methods (Static, Hidden)
    function obj = empty(varargin)
      obj = duration();
      obj.SecondsValue = double.empty(varargin{:});
    end

    function obj = fromSeconds(value, formatValue)
      obj = duration();
      obj.SecondsValue = double(value);
      if nargin > 1
        obj.FormatValue = formatValue;
      end
    end

    function secondsValue = rawSeconds(obj)
      state = builtin('struct', obj);
      secondsValue = state.SecondsValue;
    end

    function fmt = formatOf(a, b)
      % Display format for a binary set operation: taken from whichever operand
      % is a duration, preferring the first one.
      if isa(a, 'duration')
        fmt = a.FormatValue;
      else
        fmt = b.FormatValue;
      end
    end

    function s = unitInSeconds(unit)
      switch lower(char(unit))
        case 'seconds'
          s = 1;
        case 'minutes'
          s = 60;
        case 'hours'
          s = 3600;
        case 'days'
          s = 86400;
        case 'years'
          s = 365.2425 .* 86400;
        otherwise
          error('Nelson:time:unitSecondsMinutesHoursDays', 'Unit must be ''seconds'', ''minutes'', ''hours'', ''days'', or ''years''.');
      end
    end

    function out = concat(dim, varargin)
      values = cell(1, nargin - 1);
      % The result takes the format of the first operand that is a duration.
      fmt = '';
      for k = 1:numel(varargin)
        v = varargin{k};
        if isa(v, 'duration')
          if isempty(fmt)
            fmt = v.FormatValue;
          end
        elseif isa(v, 'double') || islogical(v)
          % Numbers are a count of fixed-length 24-hour days.
          values{k} = double(v) .* 86400;
          continue
        elseif isnumeric(v)
          error('Nelson:time:invalidDurationInputArgument', 'Invalid %s.', 'duration input argument');
        else
          v = duration(v);
        end
        values{k} = v.SecondsValue;
      end
      out = duration.fromSeconds(cat(dim, values{:}), fmt);
    end

    function [args, inputFormat, outputFormat] = parseNameValue(values)
      args = {};
      inputFormat = '';
      outputFormat = 'hh:mm:ss';
      k = 1;
      while k <= numel(values)
        if duration.isTextScalar(values{k})
          name = char(values{k});
          if strcmpi(name, 'InputFormat')
            if k == numel(values)
              error('Nelson:time:missingValueNameValueArgument', 'Missing value for name-value argument.');
            end
            inputFormat = duration.validateTextScalar(values{k + 1}, 'InputFormat');
            k = k + 2;
            continue
          elseif strcmpi(name, 'Format')
            if k == numel(values)
              error('Nelson:time:missingValueNameValueArgument', 'Missing value for name-value argument.');
            end
            outputFormat = duration.validateTextScalar(values{k + 1}, 'Format');
            k = k + 2;
            continue
          end
        end
        args{end + 1} = values{k};
        k = k + 1;
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

    function value = secondsFromNumericMatrix(x)
      if isempty(x)
        value = zeros(size(x));
        return
      end
      if ismatrix(x) && size(x, 2) == 3
        value = double(x(:, 1)) .* 3600 + double(x(:, 2)) .* 60 + double(x(:, 3));
        return
      end
      error('Nelson:time:numericDurationInputN3', 'Numeric duration input must be an N-by-3 matrix.');
    end

    function values = parseTextArray(text, inputFormat)
      if ischar(text)
        rows = cellstr(text);
        outSize = [size(text, 1), 1];
      elseif isstring(text)
        outSize = size(text);
        rows = cellstr(text(:));
      else
        outSize = size(text);
        rows = text(:);
      end
      values = zeros(numel(rows), 1);
      for k = 1:numel(rows)
        values(k) = duration.parseOne(rows{k}, inputFormat);
      end
      values = reshape(values, outSize);
    end

    function value = parseOne(text, inputFormat)
      text = strtrim(char(text));
      if isempty(inputFormat)
        parts = duration.splitDurationText(text);
        if numel(parts) == 3
          value = duration.partsToSeconds(parts, [3600 60 1]);
          return
        elseif numel(parts) == 4
          value = duration.partsToSeconds(parts, [86400 3600 60 1]);
          return
        end
      else
        switch lower(inputFormat)
          case 'dd:hh:mm:ss'
            value = duration.partsToSeconds(duration.splitDurationText(text), [86400 3600 60 1]);
            return
          case 'hh:mm:ss'
            value = duration.partsToSeconds(duration.splitDurationText(text), [3600 60 1]);
            return
          case 'mm:ss'
            value = duration.partsToSeconds(duration.splitDurationText(text), [60 1]);
            return
          case 'hh:mm'
            value = duration.partsToSeconds(duration.splitDurationText(text), [3600 60]);
            return
          otherwise
            if startsWith(lower(inputFormat), 'hh:mm:ss.')
              value = duration.partsToSeconds(duration.splitDurationText(text), [3600 60 1]);
              return
            elseif startsWith(lower(inputFormat), 'mm:ss.')
              value = duration.partsToSeconds(duration.splitDurationText(text), [60 1]);
              return
            end
        end
      end
      error('Nelson:time:failedConvertTextDuration', 'Failed to convert text to %s.', 'duration');
    end

    function parts = splitDurationText(text)
      signValue = 1;
      if startsWith(text, '-')
        signValue = -1;
        text = text(2:end);
      elseif startsWith(text, '+')
        error('Nelson:time:failedConvertTextDuration', 'Failed to convert text to %s.', 'duration');
      end
      raw = regexp(text, ':', 'split');
      parts = zeros(1, numel(raw));
      for k = 1:numel(raw)
        parts(k) = str2double(raw{k});
      end
      if any(isnan(parts))
        error('Nelson:time:failedConvertTextDuration', 'Failed to convert text to %s.', 'duration');
      end
      parts = parts .* signValue;
    end

    function value = partsToSeconds(parts, factors)
      if numel(parts) ~= numel(factors)
        error('Nelson:time:failedConvertTextDuration', 'Failed to convert text to %s.', 'duration');
      end
      value = sum(parts .* factors);
    end

    function [a, b, c, d] = expandInputs(a, b, c, d)
      targetSize = duration.commonSize(a, b, c, d);
      a = duration.expandToSize(a, targetSize);
      b = duration.expandToSize(b, targetSize);
      c = duration.expandToSize(c, targetSize);
      d = duration.expandToSize(d, targetSize);
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

    function [a, b] = compareInputs(a, b)
      % missing compares as NaN: never equal, never ordered.
      if isa(a, 'missing')
        a = duration(a);
      end
      if isa(b, 'missing')
        b = duration(b);
      end
      if ~isa(a, 'duration') || ~isa(b, 'duration')
        error('Nelson:time:durationOperandsExpected', '%s operands expected.', 'Duration');
      end
      [a, b] = duration.expandPair(a.SecondsValue, b.SecondsValue);
    end

    function out = formatOne(value, fmt)
      switch lower(fmt)
        case 'y'
          out = duration.formatWithUnit(value ./ (86400 * 365.2425), 'yr', 'yrs');
        case 'd'
          out = duration.formatWithUnit(value ./ 86400, 'day', 'days');
        case 'h'
          out = duration.formatWithUnit(value ./ 3600, 'hr', 'hr');
        case 'm'
          out = duration.formatWithUnit(value ./ 60, 'min', 'min');
        case 's'
          out = duration.formatWithUnit(value, 'sec', 'sec');
        otherwise
          if isnan(value)
            out = "NaN";
          elseif isinf(value)
            if value > 0
              out = "Inf";
            else
              out = "-Inf";
            end
          else
            out = duration.formatClock(value, fmt);
          end
      end
    end

    function out = formatWithUnit(value, singularUnit, pluralUnit)
      if abs(value) == 1
        unit = singularUnit;
      else
        unit = pluralUnit;
      end
      out = duration.shortNumberText(value) + " " + string(unit);
    end

    function out = shortNumberText(value)
      % Five significant digits (1.728e+05, 0.12346). Unlike '%.5g', the
      % exponent of the unrounded value picks the notation: 99999.5 gives
      % 100000, 0.0000999996 gives 1e-04.
      if value == 0 || ~isfinite(value)
        % sprintf gives NaN, Inf, -Inf; adding 0 turns -0 into 0.
        out = string(sprintf('%g', value + 0));
        return
      end
      e = floor(log10(abs(value)));
      if e < -4 || e >= 5
        txt = regexprep(sprintf('%.4e', value), '\.?0+e', 'e');
      else
        txt = sprintf(sprintf('%%.%df', 4 - e), value);
        if any(txt == '.')
          txt = regexprep(txt, '\.?0+$', '');
        end
      end
      out = string(txt);
    end

    function out = formatClock(value, fmt)
      signText = "";
      if value < 0
        signText = "-";
      end
      value = abs(value);
      totalSeconds = floor(value);
      fraction = value - totalSeconds;
      h = floor(totalSeconds / 3600);
      r = totalSeconds - h * 3600;
      mi = floor(r / 60);
      s = r - mi * 60;
      if startsWith(lower(fmt), 'dd:')
        d = floor(h / 24);
        h = h - d * 24;
        out = signText + string(sprintf('%02d:%02d:%02d:%02d', d, h, mi, s));
      elseif strcmpi(fmt, 'hh:mm')
        % Hours and minutes only; hours accumulate past 24 and the seconds
        % remainder is dropped (truncated, not rounded) like the reference.
        hh = floor(totalSeconds / 3600);
        mm = floor((totalSeconds - hh * 3600) / 60);
        out = signText + string(sprintf('%02d:%02d', hh, mm));
      elseif startsWith(lower(fmt), 'mm:')
        totalMinutes = floor(totalSeconds / 60);
        s = totalSeconds - totalMinutes * 60;
        out = signText + string(sprintf('%02d:%02d', totalMinutes, s));
      elseif contains(fmt, '.')
        ms = round(fraction * 1000);
        out = signText + string(sprintf('%02d:%02d:%02d.%03d', h, mi, s, ms));
      else
        out = signText + string(sprintf('%02d:%02d:%02d', h, mi, s));
      end
    end
  end
end
%=============================================================================
