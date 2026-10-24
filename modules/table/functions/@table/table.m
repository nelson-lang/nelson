%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef table < tabular
  properties (Hidden)
    data
    Version
    % names of the custom properties added with the 'variable' type
    VariableCustomProperties
  end

  properties
    Properties
  end

  methods
    function obj = table(varargin)
      obj.data = struct();
      obj.Version = 2;
      obj.VariableCustomProperties = {};
      obj.Properties = defaultProperties(obj);
      if nargin == 0
        return
      end

      if nargin == 1 && startsWith(class(varargin{1}), 'py.')
        [converted, convertedTable] = table.fromPandas(varargin{1});
        if converted
          obj = convertedTable;
          return
        end
      end

      if nargin == 1 && strcmp(class(varargin{1}), 'jl')
        [converted, convertedTable] = table.fromJulia(varargin{1});
        if converted
          obj = convertedTable;
          return
        end
      end

      argNames = cell(1, nargin);
      for k = 1:nargin
        argNames{k} = inputname(k);
      end
      namedArgs = normalizeNamedArguments(obj, varargin, argNames);
      parsedArgs = parseConstructorArguments(obj, namedArgs.args, namedArgs.argNames);
      dataArgs = parsedArgs.dataArgs;
      dataNames = parsedArgs.dataNames;
      opts = parsedArgs.opts;

      if ~isempty(opts.Size) || ~(isnumeric(opts.VariableTypes) && isempty(opts.VariableTypes))
        obj = preallocate(obj, opts);
        return
      end

      obj = addConstructorData(obj, dataArgs, dataNames);
      if ~isempty(opts.VariableNames)
        obj = renameAllVariables(obj, opts.VariableNames);
      end
      obj = applyConstructorProperties(obj, opts);
      obj = validateAndRefresh(obj);
    end

    function obj = set.Properties(obj, value)
      % Properties is written directly (subsasgn is bypassed). A per-variable
      % metadata edit on a table whose data matches its variable names is
      % validated and renames the data by position; internal updates in
      % which the data is ahead of the names are only stored.
      % Dynamic field: obj.Properties fails while Properties is still unset.
      current = obj.('Properties');
      if isstruct(current)
        if ~isstruct(value) || ~isequal(fieldnames(value), fieldnames(current))
          value = completeProperties(obj, checkPropertiesAssignment(obj, current, value));
          current = completeProperties(obj, current);
        end
        if ~iscell(value.RowNames)
          value.RowNames = checkedRowNames(obj, value.RowNames);
        end
        if ~isequal(value.DimensionNames, current.DimensionNames)
          value.DimensionNames = checkedDimensionNames(obj, value.DimensionNames);
          checkDimensionVariableNames(obj, value);
        end
        if isfield(value, 'CustomProperties') && isfield(current, 'CustomProperties') ...
            && ~isequal(value.CustomProperties, current.CustomProperties)
          raw = struct(obj); % objects saved before VariableCustomProperties have none
          if isfield(raw, 'VariableCustomProperties')
            value = checkVariableCustomProperties(obj, value, current, raw.VariableCustomProperties);
          end
        end
        if (~isequal(value.VariableNames, current.VariableNames) ...
            || (~isequal(value.VariableUnits, current.VariableUnits) ...
            && (~isequal(size(value.VariableUnits), size(current.VariableUnits)) || isstring(value.VariableUnits))) ...
            || (~isequal(value.VariableDescriptions, current.VariableDescriptions) ...
            && (~isequal(size(value.VariableDescriptions), size(current.VariableDescriptions)) || isstring(value.VariableDescriptions))) ...
            || (~isequal(value.VariableContinuity, current.VariableContinuity) ...
            && ~isequal(size(value.VariableContinuity), size(current.VariableContinuity)))) ...
            && isequal(fieldnames(obj.('data')), current.VariableNames(:))
          checkDimensionVariableNames(obj, value);
          checkReservedVariableNames(obj, value.VariableNames);
          [data, value] = applyVariableProperties(obj, obj.('data'), current.VariableNames, value);
          obj.data = data;
        end
      end
      obj.Properties = value;
    end

    function R = subsref(obj, sref)
      st = struct(obj);
      if strcmp(sref(1).type, '.')
        if ~ischar(sref(1).subs)
          sref = tableNumberedDot(st.Properties.VariableNames, sref, false);
        end
        field = sref(1).subs;
        if strcmp(field, 'Properties')
          R = tablePropertiesSubsref(obj, st.Properties, sref(2:end));
          return
        elseif isVariableReadyForFastRead(obj, st, field)
          R = st.data.(field);
          if length(sref) > 1
            R = subsref(R, sref(2:end));
          end
          return
        end
      end
      if ~strcmp(sref(1).type, '()') && isDataReadyForFastRead(obj, st)
        switch sref(1).type
          case '.'
            R = dotSubsref(obj, st, sref);
            return
          case '{}'
            R = braceSubsref(obj, st, sref);
            return
          case '()'
            R = parenSubsref(obj, st, sref);
            return
        end
      end
      if ~(strcmp(sref(1).type, '.') && strcmp(sref(1).subs, 'Properties'))
        obj = validateAndRefresh(obj);
      end
      st = struct(obj);
      switch sref(1).type
        case '.'
          R = dotSubsref(obj, st, sref);
        case '{}'
          R = braceSubsref(obj, st, sref);
        case '()'
          R = parenSubsref(obj, st, sref);
        otherwise
          error('Nelson:table:unsupportedSubscriptType', 'Unsupported %s type.', 'subsref');
      end
    end

    function obj = subsasgn(obj, sasgn, value)
      if length(sasgn) == 2 && strcmp(sasgn(1).type, '.') && any(strcmp(sasgn(2).type, {'()', '{}'}))
        obj = tableDotIndexSubsasgn(obj, sasgn, value);
        return
      end
      % T.Properties = value needs no struct(obj) copy. Nonempty RowNames
      % given from outside the class must name every row; a row of names
      % is checked and stored as a column.
      if isscalar(sasgn) && strcmp(sasgn.type, '.') && strcmp(sasgn.subs, 'Properties')
        try
          isNewRowNames = numel(value.RowNames) ~= numel(obj.Properties.RowNames) || size(value.RowNames, 2) > 1;
        catch
          isNewRowNames = false; % not a Properties structure: set.Properties reports it
        end
        if isNewRowNames
          value.RowNames = checkedRowNames(obj, value.RowNames);
          tableCheckRowNamesCount(value, obj.('data'));
        end
        obj.Properties = value;
        return
      end
      st = struct(obj);
      if strcmp(sasgn(1).type, '.') && ~ischar(sasgn(1).subs)
        sasgn = tableNumberedDot(st.Properties.VariableNames, sasgn, true);
      end
      switch sasgn(1).type
        case '.'
          obj = dotSubsasgn(obj, st, sasgn, value);
        case '{}'
          obj = braceSubsasgn(obj, st, sasgn, value);
        case '()'
          obj = parenSubsasgn(obj, st, sasgn, value);
        otherwise
          error('Nelson:table:unsupportedSubscriptType', 'Unsupported %s type.', 'subsasgn');
      end
    end

    function varargout = size(obj, varargin)
      st = struct(obj);
      sz = [tableHeight(obj, st), length(st.Properties.VariableNames)];
      if nargin == 1
        if nargout <= 1
          varargout{1} = sz;
        else
          for k = 1:nargout
            if k <= length(sz)
              varargout{k} = sz(k);
            else
              varargout{k} = 1;
            end
          end
        end
      else
        dim = varargin{1};
        if dim <= length(sz)
          varargout{1} = sz(dim);
        else
          varargout{1} = 1;
        end
      end
    end

    function n = end(obj, k, n)
      sz = size(obj);
      if k <= length(sz)
        n = sz(k);
      else
        n = 1;
      end
    end

    function tf = isempty(obj)
      st = struct(obj);
      tf = isempty(st.Properties.VariableNames) || tableHeight(obj, st) == 0;
    end

    function names = properties(obj)
      st = struct(obj);
      names = [st.Properties.VariableNames(:); {'Properties'}; st.Properties.DimensionNames(:)];
    end

    function R = horzcat(varargin)
      nRows = 0;
      anyRowNames = false;
      structs = cell(1, nargin);
      for k = 1:nargin
        if ~istable(varargin{k})
          error('Nelson:table:invalidType:allInputsValue', 'All inputs must be %s.', 'tables');
        end
        if k == 1
          nRows = height(varargin{1});
        elseif nRows ~= height(varargin{k})
          error('Nelson:table:dimagree:valueNumberRows', '%s must have the same number of rows.', 'All tables');
        end
        structs{k} = struct(varargin{k});
        % A table has row names when RowNames is a column (size [N 1], including
        % the empty [0 1] of a zero-row row-named table); a plain table's
        % RowNames is [0 0]. The fast path below applies only when no input
        % carries row names, so detect by shape rather than emptiness.
        if size(structs{k}.Properties.RowNames, 2) == 1
          anyRowNames = true;
        end
      end
      if anyRowNames
        % Row names require matching each table's rows to the accumulated ones,
        % done pairwise.
        st = struct(table());
        for k = 1:nargin
          st = tableHorzcatStructs(st, structs{k}, nRows);
        end
      else
        % No row names: gather every variable in one pass and build the data
        % struct once, instead of rebuilding it on every pairwise append
        % (which made [t1, ..., tN] O(numTables^2) in the total column count).
        st = struct(table());
        allNames = {};
        allVals = {};
        for k = 1:nargin
          names = structs{k}.Properties.VariableNames;
          vals = struct2cell(structs{k}.data);
          [~, loc] = ismember(names, fieldnames(structs{k}.data));
          for j = 1:numel(names)
            allNames{end + 1} = names{j};
            allVals{end + 1} = vals{loc(j)};
          end
        end
        if numel(unique(allNames)) ~= numel(allNames)
          error('Nelson:table:duplicateVariableNames', 'Duplicate table variable name.');
        end
        if ~isempty(allNames)
          st.data = __tableStructFromCell__(allVals, allNames);
        end
        st.Properties.VariableNames = allNames;
      end
      R = table.fromStruct(table.keepZeroWidthHeight(st, nRows));
      if nRows == 0 && iscolumn(st.Properties.RowNames)
        R.Properties.RowNames = cell(0, 1); % no rows, still row names
      end
    end

    function R = vertcat(varargin)
      if nargin == 0
        R = table();
        return
      end
      R = varargin{1};
      if nargin == 1
        return
      end
      if ~istable(R)
        error('Nelson:table:invalidType:allInputsValue', 'All inputs must be %s.', 'tables');
      end
      stR = struct(R);
      nR = table.heightFromStruct(stR);
      for k = 2:nargin
        if ~istable(varargin{k})
          error('Nelson:table:invalidType:allInputsValue', 'All inputs must be %s.', 'tables');
        end
        stB = struct(varargin{k});
        [stR, nR] = tableVertcatStructs(stR, nR, stB, table.heightFromStruct(stB));
      end
      R = table.fromStruct(stR);
      if nR == 0 && iscolumn(stR.Properties.RowNames)
        R.Properties.RowNames = cell(0, 1); % no rows, still row names
      end
    end

    function tf = isequal(obj, varargin)
      tf = true;
      if nargin < 2
        return
      end
      % Dispatched when any argument is a table, not only the first.
      if ~istable(obj)
        tf = false;
        return
      end
      first = struct(validateAndRefresh(obj));
      for k = 1:length(varargin)
        if ~istable(varargin{k})
          tf = false;
          return
        end
        current = struct(validateAndRefresh(varargin{k}));
        if ~builtin('isequal', first.Properties, current.Properties) ...
            || ~tableDataEqual(obj, first, current)
          tf = false;
          return
        end
      end
    end

    function [R, index] = sortrows(obj, vars, direction)
      if nargin < 2 || isempty(vars)
        vars = 1:width(obj);
      end
      if nargin < 3
        direction = 'ascend';
      end
      [R, index] = tableSortrows(obj, table.resolveVarIndices(struct(obj), vars), direction);
    end

    function R = plus(A, B)
      R = table.applyBinary(A, B, @plus);
    end

    function R = minus(A, B)
      R = table.applyBinary(A, B, @minus);
    end

    function R = times(A, B)
      R = table.applyBinary(A, B, @times);
    end

    function R = rdivide(A, B)
      R = table.applyBinary(A, B, @rdivide);
    end

    function R = ldivide(A, B)
      R = table.applyBinary(A, B, @ldivide);
    end

    function R = power(A, B)
      R = table.applyBinary(A, B, @power);
    end

    function R = eq(A, B)
      R = table.applyBinary(A, B, @eq);
    end

    function R = ne(A, B)
      R = table.applyBinary(A, B, @ne);
    end

    function R = lt(A, B)
      R = table.applyBinary(A, B, @lt);
    end

    function R = le(A, B)
      R = table.applyBinary(A, B, @le);
    end

    function R = gt(A, B)
      R = table.applyBinary(A, B, @gt);
    end

    function R = ge(A, B)
      R = table.applyBinary(A, B, @ge);
    end

    function R = and(A, B)
      R = table.applyBinary(A, B, @and);
    end

    function R = or(A, B)
      R = table.applyBinary(A, B, @or);
    end

    function R = xor(A, B)
      R = table.applyBinary(A, B, @xor);
    end

    function R = uminus(A)
      R = table.applyUnary(A, @uminus, 'uminus');
    end

    function R = not(A)
      R = table.applyUnary(A, @not, 'not');
    end

    function R = abs(A)
      R = table.applyUnary(A, @abs, 'abs');
    end

    function R = ceil(A)
      R = table.applyUnary(A, @ceil, 'ceil');
    end

    function R = fix(A)
      R = table.applyUnary(A, @fix, 'fix');
    end

    function R = floor(A)
      R = table.applyUnary(A, @floor, 'floor');
    end

    function R = round(A)
      R = table.applyUnary(A, @round, 'round');
    end

    function R = sqrt(A)
      R = table.applyUnary(A, @sqrt, 'sqrt');
    end

    function R = exp(A)
      R = table.applyUnary(A, @exp, 'exp');
    end

    function R = expm1(A)
      R = table.applyUnary(A, @expm1, 'expm1');
    end

    function R = log(A)
      R = table.applyUnary(A, @log, 'log');
    end

    function R = log10(A)
      R = table.applyUnary(A, @log10, 'log10');
    end

    function R = reallog(A)
      R = table.applyUnary(A, @reallog, 'reallog');
    end

    function R = realsqrt(A)
      R = table.applyUnary(A, @realsqrt, 'realsqrt');
    end

    function R = sin(A)
      R = table.applyUnary(A, @sin, 'sin');
    end

    function R = cos(A)
      R = table.applyUnary(A, @cos, 'cos');
    end

    function R = tan(A)
      R = table.applyUnary(A, @tan, 'tan');
    end

    function R = asind(A)
      R = table.applyUnary(A, @asind, 'asind');
    end
    function R = asinh(A)
      R = table.applyUnary(A, @asinh, 'asinh');
    end
    function R = atan(A)
      R = table.applyUnary(A, @atan, 'atan');
    end
    function R = atand(A)
      R = table.applyUnary(A, @atand, 'atand');
    end
    function R = atanh(A)
      R = table.applyUnary(A, @atanh, 'atanh');
    end
    function R = cosd(A)
      R = table.applyUnary(A, @cosd, 'cosd');
    end
    function R = cosh(A)
      R = table.applyUnary(A, @cosh, 'cosh');
    end
    function R = cospi(A)
      R = table.applyUnary(A, @cospi, 'cospi');
    end
    function R = cot(A)
      R = table.applyUnary(A, @cot, 'cot');
    end
    function R = cotd(A)
      R = table.applyUnary(A, @cotd, 'cotd');
    end
    function R = coth(A)
      R = table.applyUnary(A, @coth, 'coth');
    end
    function R = csc(A)
      R = table.applyUnary(A, @csc, 'csc');
    end
    function R = cscd(A)
      R = table.applyUnary(A, @cscd, 'cscd');
    end
    function R = csch(A)
      R = table.applyUnary(A, @csch, 'csch');
    end
    function R = log1p(A)
      R = table.applyUnary(A, @log1p, 'log1p');
    end
    function R = log2(A)
      R = table.applyUnary(A, @log2, 'log2');
    end
    function R = nextpow2(A)
      R = table.applyUnary(A, @nextpow2, 'nextpow2');
    end
    function R = sec(A)
      R = table.applyUnary(A, @sec, 'sec');
    end
    function R = secd(A)
      R = table.applyUnary(A, @secd, 'secd');
    end
    function R = sech(A)
      R = table.applyUnary(A, @sech, 'sech');
    end
    function R = sind(A)
      R = table.applyUnary(A, @sind, 'sind');
    end
    function R = sinh(A)
      R = table.applyUnary(A, @sinh, 'sinh');
    end
    function R = sinpi(A)
      R = table.applyUnary(A, @sinpi, 'sinpi');
    end
    function R = tand(A)
      R = table.applyUnary(A, @tand, 'tand');
    end
    function R = tanh(A)
      R = table.applyUnary(A, @tanh, 'tanh');
    end

    function R = atan2(A, B)
      R = table.applyBinary(A, B, @atan2);
    end

    function R = atan2d(A, B)
      R = table.applyBinary(A, B, @atan2d);
    end

    function R = nthroot(A, B)
      R = table.applyBinary(A, B, @nthroot);
    end

    function R = realpow(A, B)
      R = table.applyBinary(A, B, @realpow);
    end

    function R = sum(A, varargin)
      R = table.applyReduce(A, @sum, 'sum', varargin{:});
    end

    function R = prod(A, varargin)
      R = table.applyReduce(A, @prod, 'prod', varargin{:});
    end

    function R = mean(A, varargin)
      R = table.applyReduce(A, @mean, 'mean', varargin{:});
    end

    function R = median(A, varargin)
      R = table.applyReduce(A, @median, 'median', varargin{:});
    end

    function R = min(A, varargin)
      R = table.applyReduce(A, @min, 'min', varargin{:});
    end

    function R = max(A, varargin)
      R = table.applyReduce(A, @max, 'max', varargin{:});
    end

    function R = std(A, varargin)
      R = table.applyReduce(A, @std, 'std', varargin{:});
    end

    function varargout = var(A, varargin)
      [varargout{1:nargout}] = table.applyReduceMulti(A, @var, 'var', nargout, varargin{:});
    end

    function varargout = mode(A, varargin)
      [varargout{1:nargout}] = table.applyReduceMulti(A, @mode, 'mode', nargout, varargin{:});
    end

    function varargout = bounds(A, varargin)
      [varargout{1:nargout}] = table.applyReduceMulti(A, @bounds, 'bounds', max(nargout, 1), varargin{:});
    end

    function R = cumsum(A, varargin)
      R = table.applyMapWithArgs(A, @cumsum, 'cumsum', varargin{:});
    end

    function R = cumprod(A, varargin)
      R = table.applyMapWithArgs(A, @cumprod, 'cumprod', varargin{:});
    end

    function R = cummax(A, varargin)
      R = table.applyMapWithArgs(A, @cummax, 'cummax', varargin{:});
    end

    function R = cummin(A, varargin)
      R = table.applyMapWithArgs(A, @cummin, 'cummin', varargin{:});
    end

    function R = diff(A, varargin)
      R = table.applyMapWithArgs(A, @diff, 'diff', varargin{:});
    end

    function R = movmad(A, varargin)
      R = table.applyMapWithArgs(A, @movmad, 'movmad', varargin{:});
    end

    function R = movmax(A, varargin)
      R = table.applyMapWithArgs(A, @movmax, 'movmax', varargin{:});
    end

    function R = movmean(A, varargin)
      R = table.applyMapWithArgs(A, @movmean, 'movmean', varargin{:});
    end

    function R = movmedian(A, varargin)
      R = table.applyMapWithArgs(A, @movmedian, 'movmedian', varargin{:});
    end

    function R = movmin(A, varargin)
      R = table.applyMapWithArgs(A, @movmin, 'movmin', varargin{:});
    end

    function R = movprod(A, varargin)
      R = table.applyMapWithArgs(A, @movprod, 'movprod', varargin{:});
    end

    function R = movstd(A, varargin)
      R = table.applyMapWithArgs(A, @movstd, 'movstd', varargin{:});
    end

    function R = movsum(A, varargin)
      R = table.applyMapWithArgs(A, @movsum, 'movsum', varargin{:});
    end

    function R = movvar(A, varargin)
      R = table.applyMapWithArgs(A, @movvar, 'movvar', varargin{:});
    end
    function R = mod(A, B)
      R = table.applyBinary(A, B, @mod);
    end
    function R = rem(A, B)
      R = table.applyBinary(A, B, @rem);
    end
    function R = pow2(varargin)
      if nargin == 1
        R = table.applyUnary(varargin{1}, @pow2);
      else
        R = table.applyBinary(varargin{1}, varargin{2}, @pow2);
      end
    end
  end

  methods (Static)
    obj = empty(varargin)
    custom = variableCustomPropertiesSubset(st, order)

    function [converted, obj] = fromPandas(pyObject)
      % Build a table from a pandas DataFrame or Series. converted is false
      % (and obj is an empty table) when pyObject is not a pandas object, so
      % the constructor can fall back to its normal argument handling.
      converted = false;
      obj = table();
      [isPandas, columns, variableNames, rowNames] = __pandas_to_table_data__(pyObject);
      if ~isPandas
        return
      end
      converted = true;
      if isempty(variableNames)
        return
      end
      validNames = tableMakeValidName(variableNames);
      if isempty(rowNames)
        obj = table(columns{:}, 'VariableNames', validNames);
      else
        obj = table(columns{:}, 'VariableNames', validNames, 'RowNames', rowNames);
      end
      if ~isequal(validNames, variableNames)
        % Keep the original pandas column labels that were sanitized into valid
        % Nelson table variable names.
        obj.Properties.VariableDescriptions = variableNames;
      end
    end

    function [converted, obj] = fromJulia(jlObject)
      % Build a table from a Julia DataFrames.DataFrame. converted is false
      % (and obj is an empty table) when jlObject is not a DataFrame, so the
      % constructor can fall back to its normal argument handling.
      converted = false;
      obj = table();
      [isDataFrame, columns, variableNames, rowNames] = __julia_to_table_data__(jlObject);
      if ~isDataFrame
        return
      end
      converted = true;
      if isempty(variableNames)
        return
      end
      validNames = tableMakeValidName(variableNames);
      if isempty(rowNames)
        obj = table(columns{:}, 'VariableNames', validNames);
      else
        obj = table(columns{:}, 'VariableNames', validNames, 'RowNames', rowNames);
      end
      if ~isequal(validNames, variableNames)
        % Keep the original DataFrame column labels that were sanitized into
        % valid Nelson table variable names.
        obj.Properties.VariableDescriptions = variableNames;
      end
    end

    function obj = fromStruct(st)
      obj = table();
      if isfield(st, 'data')
        checkReservedVariableNames(obj, fieldnames(st.data));
        obj.data = st.data;
      end
      if isfield(st, 'Version')
        obj.Version = st.Version;
      else
        obj.Version = 2;
      end
      if isfield(st, 'Properties')
        obj.Properties = completeProperties(obj, st.Properties);
      end
      if isfield(st, 'VariableCustomProperties')
        obj.VariableCustomProperties = st.VariableCustomProperties;
      end
      obj = validateAndRefresh(obj);
    end

    function dispTableCompact(T)
      NB_LINES_BEFORE = 5;
      NB_LINES_AFTER = 5;
      NB_LINES_ELLIPSIS = 3;

      st = struct(T);
      varNames = st.Properties.VariableNames;
      numCols = width(T);
      numRows = height(T);
      haveRowsNames = ~isempty(st.Properties.RowNames);

      rows = [1:NB_LINES_BEFORE, numRows - NB_LINES_AFTER + 1:numRows];
      headLines = 2:NB_LINES_BEFORE + 1;
      ellipsisLines = NB_LINES_BEFORE + 2:NB_LINES_BEFORE + NB_LINES_ELLIPSIS + 1;
      tailLines = NB_LINES_BEFORE + NB_LINES_ELLIPSIS + 2:NB_LINES_BEFORE + NB_LINES_ELLIPSIS + NB_LINES_AFTER + 1;
      strs = strings(tailLines(end), numCols + 1);
      for j = 1:numCols
        strs(1, j + 1) = varNames{j};
        texts = table.displayColumnText(st.data.(varNames{j}), rows);
        strs(headLines, j + 1) = texts(1:NB_LINES_BEFORE);
        strs(ellipsisLines, j + 1) = ":";
        strs(tailLines, j + 1) = texts(NB_LINES_BEFORE + 1:end);
      end

      table.formatTableDisplay(strs, haveRowsNames);
    end

    function dispTableFull(T)
      st = struct(T);
      varNames = st.Properties.VariableNames;
      numCols = width(T);
      numRows = height(T);
      haveRowsNames = tableHasRowNames(st.Properties.RowNames, numRows);
      strs = strings(numRows + 1, numCols + 1);

      for j = 1:numCols
        strs(1, j + 1) = varNames{j};
        strs(2:end, j + 1) = table.displayColumnText(st.data.(varNames{j}), 1:numRows);
      end

      if haveRowsNames
        for i = 1:numRows
          strs{i + 1, 1} = st.Properties.RowNames{i};
        end
      end

      table.formatTableDisplay(strs, haveRowsNames);
    end

    function value = displayCellText(value)
      value = tableDisplayCellText(value);
    end

    function texts = displayColumnText(values, rows)
      texts = tableDisplayColumnText(values, rows);
    end

    function formatTableDisplay(strs, haveRowsNames)
      lines = tableDisplayLines(strs, haveRowsNames);
      for i = 1:numel(lines)
        fprintf('%s\n', lines(i));
      end
    end

    function R = applyUnary(A, fun, functionName)
      if nargin < 3
        functionName = func2str(fun);
      end
      if ~istable(A)
        error('Nelson:table:invalidType', '%s must be a table.', 'Input');
      end
      st = struct(A);
      names = st.Properties.VariableNames;
      % Read and write the variables by position. R.data.(name) per variable
      % copies the classdef data property each time, making this O(numVars^2).
      dataFields = fieldnames(st.data);
      vals = struct2cell(st.data);
      [~, loc] = ismember(names, dataFields);
      for k = 1:length(names)
        current = vals{loc(k)};
        try
          newValue = fun(current);
        catch ME
          if tablePropagatesFunctionError(functionName)
            rethrow(ME);
          end
          table.rethrowUnaryFailure(functionName, current);
        end
        if size(newValue, 1) ~= size(current, 1)
          table.rethrowUnaryFailure(functionName, current);
        end
        vals{loc(k)} = newValue;
      end
      if ~isempty(dataFields)
        st.data = __tableStructFromCell__(vals, dataFields);
      end
      st.Properties.VariableUnits = table.unaryUnits(st.Properties, functionName);
      st.Properties.VariableTypes = string([]);
      R = table.fromStruct(st);
    end

    function R = applyMapWithArgs(A, fun, functionName, varargin)
      if ~istable(A)
        error('Nelson:table:invalidType', '%s must be a table.', 'Input');
      end
      st = struct(A);
      names = st.Properties.VariableNames;
      % Read and write the variables by position: R.data.(name) per variable
      % copies the classdef data property each time, making this O(numVars^2).
      dataFields = fieldnames(st.data);
      vals = struct2cell(st.data);
      [~, loc] = ismember(names, dataFields);
      newHeight = -1;
      for k = 1:length(names)
        current = vals{loc(k)};
        try
          newValue = fun(current, varargin{:});
        catch ME
          if tablePropagatesFunctionError(functionName)
            rethrow(ME);
          end
          table.rethrowUnaryFailure(functionName, current);
        end
        if newHeight < 0
          newHeight = size(newValue, 1);
        elseif size(newValue, 1) ~= newHeight
          table.rethrowUnaryFailure(functionName, current);
        end
        vals{loc(k)} = newValue;
      end
      if ~isempty(dataFields)
        st.data = __tableStructFromCell__(vals, dataFields);
      end
      if newHeight >= 0 && ~isempty(st.Properties.RowNames) && newHeight ~= length(st.Properties.RowNames)
        st.Properties.RowNames = {};
      end
      st.Properties.VariableUnits = table.unaryUnits(st.Properties, functionName);
      st.Properties.VariableTypes = string([]);
      R = table.fromStruct(st);
    end

    function R = applyBinary(A, B, fun)
      functionName = func2str(fun);
      % Read and write the variables by position in every branch: R.data.(name)
      % per variable copies the classdef data property each time, which made
      % table arithmetic O(numVars^2) on a wide table.
      if istable(A) && istable(B)
        stA = struct(A);
        [stB, bRows] = table.alignTableStructs(stA, struct(B));
        names = stA.Properties.VariableNames;
        aFields = fieldnames(stA.data);
        aVals = struct2cell(stA.data);
        [~, aLoc] = ismember(names, aFields);
        bVals = struct2cell(stB.data);
        [~, bLoc] = ismember(names, fieldnames(stB.data));
        for k = 1:length(names)
          aValue = aVals{aLoc(k)};
          bValue = tableColumnRows(bVals{bLoc(k)}, bRows);
          try
            newValue = fun(aValue, bValue);
          catch ME
            if tablePropagatesFunctionError(functionName)
              rethrow(ME);
            end
            table.rethrowBinaryFailure(functionName, aValue);
          end
          if size(newValue, 1) ~= size(aValue, 1)
            table.rethrowBinaryFailure(functionName, aValue);
          end
          aVals{aLoc(k)} = newValue;
        end
        stR = stA;
        if ~isempty(aFields)
          stR.data = __tableStructFromCell__(aVals, aFields);
        end
        stR.Properties.VariableUnits = table.binaryUnits(stA.Properties, stB.Properties, functionName);
      elseif istable(A)
        stA = struct(A);
        names = stA.Properties.VariableNames;
        table.validateArrayOperand(B, table.heightFromStruct(stA), length(names));
        aFields = fieldnames(stA.data);
        aVals = struct2cell(stA.data);
        [~, aLoc] = ismember(names, aFields);
        for k = 1:length(names)
          aValue = aVals{aLoc(k)};
          bValue = table.arrayOperandForVariable(B, k, table.heightFromStruct(stA), length(names));
          try
            newValue = fun(aValue, bValue);
          catch ME
            if tablePropagatesFunctionError(functionName)
              rethrow(ME);
            end
            table.rethrowBinaryFailure(functionName, aValue);
          end
          if size(newValue, 1) ~= size(aValue, 1)
            table.rethrowBinaryFailure(functionName, aValue);
          end
          aVals{aLoc(k)} = newValue;
        end
        stR = stA;
        if ~isempty(aFields)
          stR.data = __tableStructFromCell__(aVals, aFields);
        end
        stR.Properties.VariableUnits = table.binaryUnits(stA.Properties, [], functionName);
      elseif istable(B)
        stB = struct(B);
        names = stB.Properties.VariableNames;
        table.validateArrayOperand(A, table.heightFromStruct(stB), length(names));
        bFields = fieldnames(stB.data);
        bVals = struct2cell(stB.data);
        [~, bLoc] = ismember(names, bFields);
        for k = 1:length(names)
          bValue = bVals{bLoc(k)};
          aValue = table.arrayOperandForVariable(A, k, table.heightFromStruct(stB), length(names));
          try
            newValue = fun(aValue, bValue);
          catch ME
            if tablePropagatesFunctionError(functionName)
              rethrow(ME);
            end
            table.rethrowBinaryFailure(functionName, bValue);
          end
          if size(newValue, 1) ~= size(bValue, 1)
            table.rethrowBinaryFailure(functionName, bValue);
          end
          bVals{bLoc(k)} = newValue;
        end
        stR = stB;
        if ~isempty(bFields)
          stR.data = __tableStructFromCell__(bVals, bFields);
        end
        stR.Properties.VariableUnits = table.binaryUnits([], stB.Properties, functionName);
      else
        R = fun(A, B);
        return
      end
      stR.Properties.VariableTypes = string([]);
      R = table.fromStruct(stR);
    end

    function R = applyReduce(A, fun, functionName, varargin)
      if ~istable(A)
        error('Nelson:table:invalidType', '%s must be a table.', 'Input');
      end
      [dim, args] = table.extractReductionDimension(varargin);
      st = struct(A);
      if dim == 2
        try
          values = A{:, :};
          result = fun(values, 2, args{:});
        catch
          table.rethrowUnaryFailure(functionName, A);
        end
        R = table(result, 'VariableNames', {functionName});
        R.Properties.RowNames = st.Properties.RowNames;
        R.Properties.VariableUnits = table.rowReductionUnits(st.Properties, functionName);
        R = validateAndRefresh(R);
        return
      end
      columns = cell(1, length(st.Properties.VariableNames));
      for k = 1:length(st.Properties.VariableNames)
        name = st.Properties.VariableNames{k};
        try
          columns{k} = fun(st.data.(name), dim, args{:});
        catch
          try
            columns{k} = fun(st.data.(name), args{:});
          catch
            table.rethrowUnaryFailure(functionName, st.data.(name));
          end
        end
      end
      R = table(columns{:}, 'VariableNames', st.Properties.VariableNames);
      R.Properties.VariableUnits = table.unaryUnits(st.Properties, functionName);
      R = validateAndRefresh(R);
    end

    function varargout = applyReduceMulti(A, fun, functionName, requestedOutputs, varargin)
      if requestedOutputs < 1
        requestedOutputs = 1;
      end
      [dim, args] = table.extractReductionDimension(varargin);
      st = struct(A);
      varargout = cell(1, requestedOutputs);
      if dim == 2
        try
          values = A{:, :};
          tmp = cell(1, requestedOutputs);
          [tmp{:}] = fun(values, dim, args{:});
        catch
          table.rethrowUnaryFailure(functionName, A);
        end
        for j = 1:requestedOutputs
          varargout{j} = table(tmp{j}, 'VariableNames', {functionName});
          varargout{j}.Properties.RowNames = st.Properties.RowNames;
          varargout{j}.Properties.VariableUnits = table.multiOutputUnits(st.Properties, functionName, j, true);
          varargout{j} = validateAndRefresh(varargout{j});
        end
        return
      end
      outputs = cell(requestedOutputs, length(st.Properties.VariableNames));
      for k = 1:length(st.Properties.VariableNames)
        name = st.Properties.VariableNames{k};
        tmp = cell(1, requestedOutputs);
        try
          [tmp{:}] = fun(st.data.(name), dim, args{:});
        catch
          try
            [tmp{:}] = fun(st.data.(name), args{:});
          catch
            table.rethrowUnaryFailure(functionName, st.data.(name));
          end
        end
        for j = 1:requestedOutputs
          outputs{j, k} = tmp{j};
        end
      end
      for j = 1:requestedOutputs
        varargout{j} = table(outputs{j, :}, 'VariableNames', st.Properties.VariableNames);
        varargout{j}.Properties.VariableUnits = table.multiOutputUnits(st.Properties, functionName, j, false);
        varargout{j} = validateAndRefresh(varargout{j});
      end
    end

    function rethrowBinaryFailure(functionName, value)
      className = class(value);
      error('Nelson:table:undefinedFunction', 'Undefined function ''%s'' for input arguments of type ''%s''.', functionName, className);
    end

    function rethrowUnaryFailure(functionName, value)
      className = class(value);
      error('Nelson:table:undefinedFunction', 'Undefined function ''%s'' for input arguments of type ''%s''.', functionName, className);
    end
  end

  methods (Static, Access = private)
    function [stB, rowsB] = alignTableStructs(stA, stB)
      namesA = stA.Properties.VariableNames;
      namesB = stB.Properties.VariableNames;
      if length(namesA) ~= length(namesB)
        error('Nelson:table:variableNamesMustMatch', '%s must match.', 'Table variable names');
      end
      order = zeros(1, length(namesA));
      for k = 1:length(namesA)
        idx = find(strcmp(namesB, namesA{k}), 1);
        if isempty(idx)
          error('Nelson:table:variableNamesMustMatch', '%s must match.', 'Table variable names');
        end
        order(k) = idx;
      end
      if ~isequal(order, 1:length(namesA))
        data = struct();
        for k = 1:length(namesA)
          data.(namesA{k}) = stB.data.(namesB{order(k)});
        end
        stB.data = data;
        stB.Properties.VariableNames = namesA;
        if length(stB.Properties.VariableTypes) == length(order)
          stB.Properties.VariableTypes = stB.Properties.VariableTypes(order);
        end
        stB.Properties.VariableUnits = table.reorderMetadata(stB.Properties.VariableUnits, order);
        stB.Properties.VariableDescriptions = table.reorderMetadata(stB.Properties.VariableDescriptions, order);
        stB.Properties.VariableContinuity = table.reorderMetadata(stB.Properties.VariableContinuity, order);
      end
      heightA = table.heightFromStruct(stA);
      heightB = table.heightFromStruct(stB);
      if heightB == 1 && heightA ~= 1
        rowsB = ones(heightA, 1);
        return
      end
      if heightA ~= heightB
        error('Nelson:table:dimagree:valueHeightOneInputOneRow', '%s must have the same height, or one input must have one row.', 'Tables');
      end
      rowsB = (1:heightB)';
      rowNamesA = stA.Properties.RowNames;
      rowNamesB = stB.Properties.RowNames;
      if ~isempty(rowNamesA) && ~isempty(rowNamesB)
        rowsB = zeros(heightA, 1);
        for k = 1:heightA
          idx = find(strcmp(rowNamesB, rowNamesA{k}), 1);
          if isempty(idx)
            error('Nelson:table:rowNamesMismatch', '%s must match.', 'Table row names');
          end
          rowsB(k) = idx;
        end
      end
    end

    function value = reorderMetadata(value, order)
      if isempty(value)
        return
      end
      if length(value) == length(order)
        value = value(order);
      end
    end

    function n = heightFromStruct(st)
      % A table without variables keeps its height in the size of its
      % field-less data struct array (n-by-0); the scalar struct() means 0.
      names = st.Properties.VariableNames;
      if ~isempty(names)
        n = size(st.data.(names{1}), 1);
      elseif isempty(st.data)
        n = size(st.data, 1);
      else
        n = 0;
      end
    end

    function st = keepZeroWidthHeight(st, nRows)
      if isempty(st.Properties.VariableNames)
        st.data = repmat(struct(), nRows, 0);
      end
    end

    function validateArrayOperand(value, nRows, nVars)
      if ~(isnumeric(value) || islogical(value))
        error('Nelson:table:invalidType:otherOperandNumericLogical', 'Other operand must be numeric or logical.');
      end
      if ndims(value) > 2
        error('Nelson:table:notSupported', 'Multidimensional array operands are not supported.');
      end
      if isscalar(value)
        return
      end
      sz = size(value);
      if isrow(value) && length(value) == nVars
        return
      end
      if iscolumn(value) && length(value) == nRows
        return
      end
      if isequal(sz, [nRows, nVars])
        return
      end
      error('Nelson:table:dimagree:arrayOperandSizeCompatibleTable', 'Array operand size must be compatible with the table.');
    end

    function value = arrayOperandForVariable(value, k, nRows, nVars)
      if isscalar(value)
        return
      end
      if isrow(value) && length(value) == nVars
        value = value(k);
      elseif iscolumn(value) && length(value) == nRows
        value = value(:);
      else
        value = value(:, k);
      end
    end

    function [dim, args] = extractReductionDimension(args)
      dim = 1;
      if ~isempty(args) && isnumeric(args{1}) && isscalar(args{1})
        dim = args{1};
        args = args(2:end);
      end
    end

    function units = unaryUnits(props, functionName)
      units = tableNormalizedUnits(props);
      if isempty(units)
        return
      end
      if ~table.functionPreservesUnits(functionName)
        units = repmat({''}, 1, length(props.VariableNames));
      end
      units = table.collapseEmptyUnits(units);
    end

    function units = rowReductionUnits(props, functionName)
      units = tableNormalizedUnits(props);
      if isempty(units)
        return
      end
      if ~table.functionPreservesUnits(functionName)
        units = {};
        return
      end
      defined = units(~cellfun('isempty', units));
      if isempty(defined)
        units = {};
        return
      end
      first = defined{1};
      for k = 1:length(defined)
        if ~strcmp(first, defined{k})
          error('Nelson:table:incompatibleUnits', 'Table variable units must be compatible.');
        end
      end
      units = {first};
    end

    function units = multiOutputUnits(props, functionName, outputIndex, isRowReduction)
      if strcmp(functionName, 'mode')
        if outputIndex == 1 || outputIndex == 3
          if isRowReduction
            units = table.rowReductionUnits(props, functionName);
          else
            units = table.unaryUnits(props, functionName);
          end
        else
          units = table.emptyUnitsForProperties(props, isRowReduction);
        end
      elseif strcmp(functionName, 'var')
        if outputIndex == 2
          if isRowReduction
            units = table.rowReductionUnits(props, 'mean');
          else
            units = table.unaryUnits(props, 'mean');
          end
        else
          units = table.emptyUnitsForProperties(props, isRowReduction);
        end
      else
        if isRowReduction
          units = table.rowReductionUnits(props, functionName);
        else
          units = table.unaryUnits(props, functionName);
        end
      end
    end

    function units = emptyUnitsForProperties(props, isRowReduction)
      units = {};
    end

    function units = binaryUnits(propsA, propsB, functionName)
      if isempty(propsA)
        unitsA = repmat({''}, 1, length(propsB.VariableNames));
        unitsB = tableNormalizedUnits(propsB);
        n = length(propsB.VariableNames);
      elseif isempty(propsB)
        unitsA = tableNormalizedUnits(propsA);
        unitsB = repmat({''}, 1, length(propsA.VariableNames));
        n = length(propsA.VariableNames);
      else
        unitsA = tableNormalizedUnits(propsA);
        unitsB = tableNormalizedUnits(propsB);
        n = length(propsA.VariableNames);
      end
      units = repmat({''}, 1, n);
      for k = 1:n
        units{k} = table.combineUnits(unitsA{k}, unitsB{k}, functionName);
      end
      units = table.collapseEmptyUnits(units);
    end

    function unit = combineUnits(unitA, unitB, functionName)
      aDefined = ~isempty(unitA);
      bDefined = ~isempty(unitB);
      switch functionName
        case {'plus', 'minus', 'mod', 'rem'}
          unit = table.compatibleUnits(unitA, unitB, true);
        case {'eq', 'ne', 'lt', 'le', 'gt', 'ge', 'and', 'or', 'xor', 'atan2', 'atan2d', 'nthroot', 'realpow'}
          table.compatibleUnits(unitA, unitB, false);
          unit = '';
        case 'times'
          if aDefined && xor(aDefined, bDefined)
            tableWarnUnitMismatch();
            unit = unitA;
          elseif bDefined && xor(aDefined, bDefined)
            tableWarnUnitMismatch();
            unit = unitB;
          else
            unit = '';
          end
        case 'rdivide'
          if aDefined && ~bDefined
            tableWarnUnitMismatch();
            unit = unitA;
          else
            unit = '';
          end
        case 'ldivide'
          if aDefined && ~bDefined
            tableWarnUnitMismatch();
            unit = unitA;
          else
            unit = '';
          end
        case 'power'
          if aDefined && bDefined
            error('Nelson:table:incompatibleUnits', 'Table variable units must be compatible.');
          end
          unit = '';
        otherwise
          unit = '';
      end
    end

    function unit = compatibleUnits(unitA, unitB, preserve)
      aDefined = ~isempty(unitA);
      bDefined = ~isempty(unitB);
      if aDefined && bDefined && ~strcmp(unitA, unitB)
        error('Nelson:table:incompatibleUnits', 'Table variable units must be compatible.');
      end
      unit = '';
      if preserve
        if aDefined && bDefined
          unit = unitA;
        elseif aDefined
          tableWarnUnitMismatch();
          unit = unitA;
        elseif bDefined
          tableWarnUnitMismatch();
          unit = unitB;
        end
      end
    end

    function tf = functionPreservesUnits(functionName)
      tf = any(strcmp(functionName, {'uplus', 'uminus', 'abs', 'ceil', 'fix', 'floor', ...
        'round', 'sum', 'mean', 'median', 'min', 'max', 'mode', 'std', 'cumsum', ...
        'cummax', 'cummin', 'movmad', 'movmax', 'movmean', 'movmedian', 'movmin', ...
        'movstd', 'movsum'}));
    end

    function units = collapseEmptyUnits(units)
      if ~isempty(units) && all(cellfun('isempty', units))
        units = {};
      end
    end

    function idx = resolveVarIndices(st, vars)
      names = st.Properties.VariableNames;
      if nargin < 2 || isempty(vars) || (ischar(vars) && strcmp(vars, ':'))
        idx = 1:length(names);
        return
      end
      if strcmp(class(vars), 'vartype')
        idx = find(strcmp(st.Properties.VariableTypes, vars.TypeName));
        return
      end
      if isa(vars, 'function_handle')
        idx = [];
        for k = 1:length(names)
          try
            tf = vars(st.Properties.VariableTypes(k));
          catch
            tf = vars(st.data.(names{k}));
          end
          if tf
            idx(end + 1) = k;
          end
        end
        return
      end
      if islogical(vars)
        idx = find(vars);
        return
      end
      if isnumeric(vars)
        idx = vars;
        return
      end
      if isstring(vars)
        vars = cellstr(vars);
      elseif ischar(vars)
        vars = {vars};
      end
      idx = zeros(1, length(vars));
      for k = 1:length(vars)
        found = find(strcmp(names, vars{k}), 1);
        if isempty(found)
          error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), vars{k});
        end
        idx(k) = found;
      end
    end
  end

  methods (Access = private)
    function props = defaultProperties(obj)
      props = struct();
      props.DimensionNames = {'Row', 'Variables'};
      props.Description = '';
      props.UserData = [];
      props.VariableNames = {};
      props.VariableTypes = string([]);
      props.VariableDescriptions = {};
      props.VariableUnits = {};
      props.VariableContinuity = [];
      props.RowNames = {};
      props.CustomProperties = struct();
    end

    function props = completeProperties(obj, props)
      defaults = defaultProperties(obj);
      names = fieldnames(defaults);
      orderedProps = struct();
      for k = 1:length(names)
        if isfield(props, names{k})
          orderedProps.(names{k}) = props.(names{k});
        else
          orderedProps.(names{k}) = defaults.(names{k});
        end
      end
      extraNames = fieldnames(props);
      for k = 1:length(extraNames)
        if ~isfield(orderedProps, extraNames{k})
          orderedProps.(extraNames{k}) = props.(extraNames{k});
        end
      end
      props = orderedProps;
      props.VariableNames = toCellstrRow(obj, props.VariableNames);
      props.RowNames = toCellstrColumn(obj, props.RowNames);
      props.DimensionNames = toCellstrRow(obj, props.DimensionNames);
      if length(props.DimensionNames) ~= 2
        props.DimensionNames = defaults.DimensionNames;
      end
      if iscellstr(props.VariableTypes)
        props.VariableTypes = string(props.VariableTypes);
      end
      if isempty(props.VariableTypes)
        props.VariableTypes = string([]);
      end
    end

    function result = normalizeNamedArguments(obj, argsIn, argNamesIn)
      result = struct();
      result.args = argsIn;
      result.argNames = argNamesIn;
      if ~isempty(result.args) && isstruct(result.args{end}) && isscalar(result.args{end})
        named = namedargs2cell(result.args{end});
        result.args = [result.args(1:end - 1), named];
        result.argNames = [result.argNames(1:end - 1), cell(1, length(named))];
      end
    end

    function result = parseConstructorArguments(obj, args, argNames)
      result = struct();
      result.opts = defaultConstructorOptions(obj);
      % Accumulate into local cells, not struct fields: result.dataArgs{end+1}
      % reads and rewrites the whole cell through the struct on every append,
      % which made parsing a wide table's arguments O(numArgs^2).
      dataArgs = {};
      dataNames = {};
      nArgNames = length(argNames);
      k = 1;
      while k <= length(args)
        option = isConstructorOption(obj, args{k});
        if option.isOption
          if k == length(args)
            error('Nelson:table:nameValuePair:nameValueArgumentFollowedByA', 'Name-value argument must be followed by a value.');
          end
          result.opts.(option.name) = args{k + 1};
          k = k + 2;
        else
          dataArgs{end + 1} = args{k};
          if k <= nArgNames
            dataNames{end + 1} = argNames{k};
          else
            dataNames{end + 1} = '';
          end
          k = k + 1;
        end
      end
      result.dataArgs = dataArgs;
      result.dataNames = dataNames;
    end

    function opts = defaultConstructorOptions(obj)
      opts = struct();
      opts.VariableNames = {};
      opts.RowNames = {};
      opts.DimensionNames = {};
      opts.Description = [];
      opts.UserData = [];
      opts.VariableDescriptions = [];
      opts.VariableUnits = [];
      opts.VariableContinuity = [];
      opts.Size = [];
      opts.VariableTypes = [];
    end

    function option = isConstructorOption(obj, value)
      option = struct();
      option.isOption = false;
      option.name = '';
      if isstring(value)
        if ~isscalar(value)
          return
        end
        value = char(value);
      end
      if ~ischar(value)
        return
      end
      if ~isrow(value)
        return
      end
      optionNames = {'VariableNames', 'RowNames', 'DimensionNames', 'Description', ...
        'UserData', 'VariableDescriptions', 'VariableUnits', 'VariableContinuity', ...
        'Size', 'VariableTypes'};
      idx = find(strcmpi(value, optionNames));
      if ~isempty(idx)
        option.isOption = true;
        option.name = optionNames{idx(1)};
      end
    end

    function obj = addConstructorData(obj, dataArgs, dataNames)
      % Each data argument is one variable, stored as given: a cell array
      % stays a cell variable (cell2table is the one that unwraps cells).
      % Build the data struct and the names in one pass: assigning
      % obj.data.(name) and growing obj.Properties.VariableNames per column
      % reads and rewrites the whole classdef property each time, which made
      % constructing a wide table O(numVars^2).
      namePrefix = 'Var';
      if ~isempty(dataNames) && ~isempty(dataNames{1})
        namePrefix = dataNames{1};
      end
      n = length(dataArgs);
      names = cell(1, n);
      for j = 1:n
        if isempty(dataNames{j})
          names{j} = [namePrefix, num2str(j)];
        else
          names{j} = dataNames{j};
        end
      end
      if n > 0
        checkReservedVariableNames(obj, names);
        obj.data = __tableStructFromCell__(dataArgs(:), names(:));
        obj.Properties.VariableNames = names;
      end
    end

    function obj = addVariable(obj, name, value)
      checkReservedVariableNames(obj, name);
      obj.data.(name) = value;
      obj.Properties.VariableNames{end + 1} = name;
    end

    function obj = renameAllVariables(obj, newVariableNames)
      newVariableNames = checkedVariableNames(obj, newVariableNames);
      if length(newVariableNames) ~= length(obj.Properties.VariableNames)
        error('Nelson:table:variableNamesMismatch', 'Mismatched variable names.');
      end
      validateNames(obj, newVariableNames, sprintf('%s names must be nonempty and distinct.', 'Variable'));
      checkReservedVariableNames(obj, newVariableNames);
      % Rebuild the data struct in one pass. Reading obj.data by dynamic field
      % name per variable copies the whole backing struct each time, making a
      % wide rename O(numVars^2).
      data = obj.('data');
      oldNames = obj.Properties.VariableNames;
      vals = struct2cell(data);
      [~, loc] = ismember(oldNames, fieldnames(data));
      obj.data = __tableStructFromCell__(vals(loc), newVariableNames(:));
      obj.Properties.VariableNames = newVariableNames;
    end

    function obj = applyConstructorProperties(obj, opts)
      if ~isempty(opts.RowNames)
        obj.Properties.RowNames = checkedRowNames(obj, opts.RowNames);
      end
      if ~isempty(opts.DimensionNames)
        obj.Properties.DimensionNames = checkedDimensionNames(obj, opts.DimensionNames);
      end
      if ~isempty(opts.Description)
        obj.Properties.Description = opts.Description;
      end
      if ~isempty(opts.UserData)
        obj.Properties.UserData = opts.UserData;
      end
      if ~isempty(opts.VariableDescriptions)
        obj.Properties.VariableDescriptions = toCellstrRow(obj, opts.VariableDescriptions);
      end
      if ~isempty(opts.VariableUnits)
        obj.Properties.VariableUnits = toCellstrRow(obj, opts.VariableUnits);
      end
      if ~isempty(opts.VariableContinuity)
        obj.Properties.VariableContinuity = opts.VariableContinuity;
      end
      obj.Properties = completeProperties(obj, obj.Properties);
    end

    function obj = preallocate(obj, opts)
      if isempty(opts.Size) || (isnumeric(opts.VariableTypes) && isempty(opts.VariableTypes))
        error('Nelson:table:invalidSyntax:sizeVariableTypesSpecifiedTogether', 'Size and VariableTypes must be specified together.');
      end
      if ~isnumeric(opts.Size) || length(opts.Size) ~= 2
        error('Nelson:table:invalidValue:sizeATwoElementNumericVector', 'Size must be a two-element numeric vector.');
      end
      nRows = opts.Size(1);
      nVars = opts.Size(2);
      variableTypes = toCellstrRow(obj, opts.VariableTypes);
      if length(variableTypes) ~= nVars
        error('Nelson:table:dimagree:valueMatchNumberVariables', '%s must match the number of variables.', 'VariableTypes');
      end
      if isempty(opts.VariableNames)
        variableNames = cell(1, nVars);
        for k = 1:nVars
          variableNames{k} = ['Var', num2str(k)];
        end
      else
        variableNames = checkedVariableNames(obj, opts.VariableNames);
      end
      if length(variableNames) ~= nVars
        error('Nelson:table:dimagree:valueMatchNumberVariables', '%s must match the number of variables.', 'VariableNames');
      end
      obj.data = struct();
      obj.Properties = defaultProperties(obj);
      for k = 1:nVars
        obj = addVariable(obj, variableNames{k}, initialValue(obj, nRows, variableTypes{k}));
      end
      obj = table.keepZeroWidthHeight(obj, nRows);
      obj = applyConstructorProperties(obj, opts);
      obj = validateAndRefresh(obj);
    end

    function value = initialValue(obj, nRows, typeName)
      switch lower(typeName)
        case {'double', 'doublenan'}
          value = zeros(nRows, 1);
          if contains(lower(typeName), 'nan')
            value(:) = NaN;
          end
        case 'single'
          value = single(zeros(nRows, 1));
        case 'logical'
          value = false(nRows, 1);
        case 'string'
          value = strings(nRows, 1);
        case {'cell', 'cellstr', 'char'}
          value = cell(nRows, 1);
          for k = 1:nRows
            value{k} = '';
          end
        case 'int8'
          value = int8(zeros(nRows, 1));
        case 'int16'
          value = int16(zeros(nRows, 1));
        case 'int32'
          value = int32(zeros(nRows, 1));
        case 'int64'
          value = int64(zeros(nRows, 1));
        case 'uint8'
          value = uint8(zeros(nRows, 1));
        case 'uint16'
          value = uint16(zeros(nRows, 1));
        case 'uint32'
          value = uint32(zeros(nRows, 1));
        case 'uint64'
          value = uint64(zeros(nRows, 1));
        otherwise
          error('Nelson:table:unsupportedVariableType', 'Unsupported variable type: %s', typeName);
      end
    end

    function obj = validateAndRefresh(obj)
      obj.Properties = completeProperties(obj, obj.Properties);
      validateNames(obj, obj.Properties.VariableNames, sprintf('%s names must be nonempty and distinct.', 'Variable'));
      checkDimensionVariableNames(obj, obj.Properties);
      data = obj.('data'); % obj.data would read a variable named data
      dataNames = fieldnames(data);
      names = obj.Properties.VariableNames;
      if length(dataNames) == length(names) && ~isempty(names)
        if ~all(ismember(names, dataNames))
          % The data fields do not match the variable names: rename them by
          % position in one pass. The former per-name isfield check and
          % field-by-field rebuild were each O(numVars^2).
          data = __tableStructFromCell__(struct2cell(data), names(:));
          dataNames = fieldnames(data);
        end
      end
      if length(dataNames) > length(names)
        extra = dataNames(~ismember(dataNames, names));
        names = [names, extra(:)'];
        obj.Properties.VariableNames = names;
      end
      if ~isempty(names)
        % Read the variables by position. data.(names{k}) looks a field up
        % with a linear scan, so this per-variable loop would be O(numVars^2).
        heightVals = struct2cell(data);
        [~, heightLoc] = ismember(names, fieldnames(data));
        nRows = variableHeight(obj, heightVals{heightLoc(1)});
        for k = 2:length(names)
          if ~isequal(nRows, variableHeight(obj, heightVals{heightLoc(k)}))
            error('Nelson:table:dimagree:valueNumberRows', '%s must have the same number of rows.', 'All table variables');
          end
        end
        if ~isempty(obj.Properties.RowNames) && length(obj.Properties.RowNames) ~= nRows
          error('Nelson:table:dimagree', '%s must match the number of rows.', 'RowNames');
        end
      elseif ~isempty(obj.Properties.RowNames)
        if ~isempty(data)
          data = repmat(struct(), length(obj.Properties.RowNames), 0);
        elseif size(data, 1) ~= length(obj.Properties.RowNames)
          error('Nelson:table:dimagree', '%s must match the number of rows.', 'RowNames');
        end
      end
      requestedTypes = obj.Properties.VariableTypes;
      if length(requestedTypes) == length(names)
        % Read the variables by position. data.(names{k}) looks a field up
        % with a linear scan, so this per-variable loop would be O(numVars^2)
        % even when no variable actually needs casting.
        castNames = fieldnames(data);
        castVals = struct2cell(data);
        [~, castLoc] = ismember(names, castNames);
        castChanged = false;
        for k = 1:length(names)
          requestedType = char(requestedTypes(k));
          current = castVals{castLoc(k)};
          if ~isempty(requestedType) && ~strcmp(requestedType, class(current))
            castVals{castLoc(k)} = castVariable(obj, current, requestedType);
            castChanged = true;
          end
        end
        if castChanged
          data = __tableStructFromCell__(castVals, castNames);
        end
      end
      obj.data = data;
      obj.Properties.VariableTypes = variableTypes(obj, data);
      obj.Version = 2;
    end

    function value = castVariable(obj, value, typeName)
      switch lower(typeName)
        case 'double'
          value = double(value);
        case 'single'
          value = single(value);
        case 'logical'
          value = logical(value);
        case 'string'
          value = string(value);
        case 'cell'
          if ~iscell(value)
            value = num2cell(value);
          end
        case 'int8'
          value = int8(value);
        case 'int16'
          value = int16(value);
        case 'int32'
          value = int32(value);
        case 'int64'
          value = int64(value);
        case 'uint8'
          value = uint8(value);
        case 'uint16'
          value = uint16(value);
        case 'uint32'
          value = uint32(value);
        case 'uint64'
          value = uint64(value);
        otherwise
          % Columns of a non-built-in class (graphics handles, user objects,
          % categorical, ...) have no numeric cast to apply and a table may
          % hold any class, so the value is kept as-is. The recorded type is
          % refreshed from the actual data afterwards. This matters for
          % vertcat, where concatenating a subclass column (for example
          % GraphicsPlaceholder) can widen it to its base class.
      end
    end

    function types = variableTypes(obj, data)
      names = obj.Properties.VariableNames;
      n = length(names);
      if n == 0
        types = string([]);
        return
      end
      % Read the variables by position. data.(names{k}) looks a field up with
      % a linear scan, so a per-variable loop would be O(numVars^2).
      vals = struct2cell(data);
      [~, loc] = ismember(names, fieldnames(data));
      types = strings(1, n);
      for k = 1:n
        types(k) = class(vals{loc(k)});
      end
    end

    function tf = isVariableReadyForFastRead(obj, st, name)
      idx = find(strcmp(st.Properties.VariableNames, name), 1);
      tf = false;
      if isempty(idx) || ~isfield(st.data, name)
        return
      end
      requestedTypes = st.Properties.VariableTypes;
      if length(requestedTypes) == length(st.Properties.VariableNames)
        requestedType = char(requestedTypes(idx));
        if ~isempty(requestedType) && ~strcmp(requestedType, class(st.data.(name)))
          return
        end
      end
      tf = true;
    end

    function tf = isDataReadyForFastRead(obj, st)
      names = st.Properties.VariableNames;
      tf = true;
      for k = 1:length(names)
        if ~isfield(st.data, names{k})
          tf = false;
          return
        end
      end
      requestedTypes = st.Properties.VariableTypes;
      if length(requestedTypes) == length(names)
        for k = 1:length(names)
          requestedType = char(requestedTypes(k));
          if ~isempty(requestedType) && ~strcmp(requestedType, class(st.data.(names{k})))
            tf = false;
            return
          end
        end
      end
    end

    function R = dotSubsref(obj, st, sref)
      field = sref(1).subs;
      if strcmp(field, 'Properties')
        R = st.Properties;
      elseif any(strcmp(st.Properties.VariableNames, field))
        R = st.data.(field);
      elseif strcmp(field, st.Properties.DimensionNames{1})
        R = st.Properties.RowNames(:);
      elseif strcmp(field, st.Properties.DimensionNames{2})
        R = braceSubsref(obj, st, struct('type', '{}', 'subs', {{':', ':'}}));
      elseif isfield(st.Properties, field)
        error('Nelson:table:illegalPropertyReference', _('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), field, field);
      else
        error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), field);
      end
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function R = braceSubsref(obj, st, sref)
      if length(sref(1).subs) ~= 2
        error('Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts', '%s indexing requires row and variable subscripts.', 'Brace');
      end
      nRows = tableHeight(obj, st);
      rows = tableResolveRows(st, sref(1).subs{1}, nRows);
      vars = resolveVars(obj, st, sref(1).subs{2});
      tableCheckRowIndex(rows, nRows);
      R = [];
      for k = 1:length(vars)
        name = st.Properties.VariableNames{vars(k)};
        value = sliceRows(obj, st.data.(name), rows);
        if isempty(R)
          R = value;
        else
          R = [R, value];
        end
      end
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function R = parenSubsref(obj, st, sref)
      if length(sref(1).subs) ~= 2
        error('Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts', '%s indexing requires row and variable subscripts.', 'Parentheses');
      end
      nRows = tableHeight(obj, st);
      rows = tableResolveRows(st, sref(1).subs{1}, nRows);
      vars = resolveVars(obj, st, sref(1).subs{2});
      tableCheckRowIndex(rows, nRows);
      if isempty(vars)
        R = table();
        R.data = repmat(struct(), numel(rows), 0);
        if ~isempty(st.Properties.RowNames)
          R.Properties.RowNames = tableRenameRepeated(st.Properties.RowNames(rows), rows);
        end
        R.Properties.DimensionNames = st.Properties.DimensionNames;
        R.Properties.Description = st.Properties.Description;
        R.Properties.UserData = st.Properties.UserData;
        R = validateAndRefresh(R);
      else
        R = subsetTable(obj, st, rows, vars);
      end
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function R = subsetTable(obj, st, rows, vars)
      st.Properties = completeProperties(obj, st.Properties);
      vars = vars(:)';
      sourceNames = st.Properties.VariableNames;
      names = sourceNames(vars);
      names = tableRenameRepeated(names(:)', vars, namelengthmax());
      data = struct();
      for k = 1:length(names)
        data.(names{k}) = sliceRows(obj, st.data.(sourceNames{vars(k)}), rows);
      end
      % per-variable metadata set (one entry per variable) follows the
      % selection; the properties are assigned at once to stay consistent
      props = st.Properties;
      props.VariableNames = names;
      perVariable = {'VariableTypes', 'VariableDescriptions', 'VariableUnits', 'VariableContinuity'};
      for k = 1:length(perVariable)
        field = perVariable{k};
        if numel(props.(field)) == numel(sourceNames)
          props.(field) = reshape(props.(field)(vars), 1, []);
        end
      end
      props.CustomProperties = table.variableCustomPropertiesSubset(st, vars);
      if ~isempty(props.RowNames)
        props.RowNames = tableRenameRepeated(props.RowNames(rows), rows);
      end
      R = obj;
      R.data = data;
      R.Version = 2;
      R.Properties = props;
      if length(props.VariableTypes) ~= length(names)
        R.Properties.VariableTypes = variableTypes(R);
      end
    end

    function obj = dotSubsasgn(obj, st, sasgn, value)
      if length(sasgn) ~= 1
        error('Nelson:table:unsupportedAssignment', 'Unsupported %s assignment.', 'table');
      end
      field = sasgn(1).subs; % .Properties is handled by subsasgn
      if strcmp(field, 'RowNames') || strcmp(field, 'VariableNames')
        error('Nelson:table:invalidPropertyAssignment', _('Assign to the ''%s'' property of a table using dot subscripting via .Properties.%s.'), field, field);
      end
      nRows = tableHeight(obj, st);
      if isempty(value) && isequal(size(value), [0 0])
        idx = find(strcmp(st.Properties.VariableNames, field));
        if isempty(idx)
          error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), field);
        end
        st = tableDeleteVariables(st, idx);
        obj = table.fromStruct(table.keepZeroWidthHeight(st, nRows));
        return
      end
      if isempty(st.Properties.VariableNames)
        st.data = struct();
        if nRows == 0
          nRows = size(value, 1);
        end
      end
      if any(strcmp(st.Properties.VariableNames, field)) && ~isequal(size(value), size(st.data.(field)))
        error('Nelson:table:dimagree:valueAssignmentSizeExistingValue', 'Value assignment must be same size as existing value.');
      end
      if size(value, 1) ~= nRows
        error('Nelson:table:dimagree:valueAssignmentNumberRowsTable', 'Value assignment must have the same number of rows as the table.');
      end
      if ~any(strcmp(st.Properties.VariableNames, field))
        st.Properties.VariableNames{end + 1} = field;
      end
      st.data.(field) = value;
      st.Properties.VariableTypes = string([]);
      obj = table.fromStruct(st);
    end

    function obj = braceSubsasgn(obj, st, sasgn, value)
      if length(sasgn(1).subs) ~= 2
        error('Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts', '%s indexing requires row and variable subscripts.', 'Brace');
      end
      nRows = tableHeight(obj, st);
      [rows, newRowNames] = tableResolveRows(st, sasgn(1).subs{1}, nRows, true);
      varSub = sasgn(1).subs{2};
      if isempty(value) && isequal(size(value), [0 0])
        error('Nelson:table:invalidAssignment', 'To delete rows or variables by assigning [], use () subscripting instead of {}.');
      end
      if isnumeric(varSub) && any(varSub(:) > length(st.Properties.VariableNames))
        varSub = tableNumberedNewVariables(st.Properties.VariableNames, varSub);
      end
      if isstring(varSub) || iscellstr(varSub) || (ischar(varSub) && ~strcmp(varSub, ':'))
        [st, rows] = tableBraceNewVariables(st, toCellstrRow(obj, varSub), rows, nRows, value, sasgn(1).subs{1});
      end
      vars = resolveVars(obj, st, varSub);
      for k = 1:length(vars)
        name = st.Properties.VariableNames{vars(k)};
        col = st.data.(name);
        try
          if length(vars) == 1
            col(rows, :) = value;
          else
            col(rows, :) = value(:, k);
          end
        catch
          error('Nelson:table:dimagree:valueAssignmentSizeExistingValue', 'Value assignment must be same size as existing value.');
        end
        st.data.(name) = col;
      end
      if max([rows, 0]) > nRows
        st = tableGrowRows(st, nRows, st.Properties.VariableNames(vars), newRowNames);
      end
      st.Properties.VariableTypes = string([]);
      obj = table.fromStruct(st);
    end

    function obj = parenSubsasgn(obj, st, sasgn, value)
      if length(sasgn(1).subs) ~= 2
        error('Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts', '%s indexing requires row and variable subscripts.', 'Parentheses');
      end
      nRows = tableHeight(obj, st);
      isDeletion = isa(value, 'double') && ~issparse(value) && isreal(value) && isequal(size(value), [0 0]); % only [] deletes
      [rows, newRowNames] = tableResolveRows(st, sasgn(1).subs{1}, nRows, ~isDeletion);
      varSub = sasgn(1).subs{2};
      if isDeletion
        vars = resolveVars(obj, st, varSub);
        if ischar(sasgn(1).subs{1}) && strcmp(sasgn(1).subs{1}, ':')
          st = tableDeleteVariables(st, vars);
        elseif ischar(sasgn(1).subs{2}) && strcmp(sasgn(1).subs{2}, ':')
          keep = true(nRows, 1);
          keep(rows) = false;
          nRows = nnz(keep);
          for k = 1:length(st.Properties.VariableNames)
            name = st.Properties.VariableNames{k};
            st.data.(name) = sliceRows(obj, st.data.(name), keep);
          end
          if ~isempty(st.Properties.RowNames)
            st.Properties.RowNames = st.Properties.RowNames(keep);
          end
        else
          error('Nelson:table:invalidSubscript:leastOneSubscriptWhenYouDelete', 'At least one subscript must be '':'' when you delete rows or variables by assigning [].');
        end
        obj = table.fromStruct(table.keepZeroWidthHeight(st, nRows));
        if nRows == 0 && iscolumn(st.Properties.RowNames)
          obj.Properties.RowNames = cell(0, 1); % no rows left, still row names
        end
        return
      end
      value = tableParenAssignedValue(value, rows, varSub, st.Properties.VariableNames);
      valueSt = struct(value);
      if isnumeric(varSub) && any(varSub(:) > length(st.Properties.VariableNames))
        varSub = tableNumberedNewVariables(st.Properties.VariableNames, varSub);
      end
      try
        vars = resolveVars(obj, st, varSub);
      catch err
        if ~strcmp(err.identifier, 'Nelson:table:unrecognizedVariable')
          rethrow(err);
        end
        [st, rows] = tableParenNewVariables(st, toCellstrRow(obj, varSub), rows, nRows, valueSt, sasgn(1).subs{1});
        vars = resolveVars(obj, st, varSub);
      end
      for k = 1:length(vars)
        name = st.Properties.VariableNames{vars(k)};
        sourceName = valueSt.Properties.VariableNames{k};
        col = st.data.(name);
        col = tableAssignVariableRows(col, rows, valueSt.data.(sourceName), name);
        st.data.(name) = col;
      end
      if max([rows, 0]) > nRows
        st = tableGrowRows(st, nRows, st.Properties.VariableNames(vars), newRowNames);
      end
      st.Properties.VariableTypes = string([]);
      obj = table.fromStruct(st);
    end

    function vars = resolveVars(obj, st, sub)
      if ischar(sub) && strcmp(sub, ':')
        vars = 1:length(st.Properties.VariableNames);
      elseif islogical(sub)
        vars = find(sub);
      elseif isnumeric(sub)
        vars = sub;
      elseif ischar(sub) || isstring(sub) || iscellstr(sub)
        names = toCellstrRow(obj, sub);
        vars = zeros(1, length(names));
        for k = 1:length(names)
          idx = find(strcmp(st.Properties.VariableNames, names{k}));
          if isempty(idx)
            error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), names{k});
          end
          vars(k) = idx(1);
        end
      else
        error('Nelson:table:invalidSubscript', 'Invalid %s subscript type.', 'variable');
      end
      if ~isempty(vars) && max(vars) > length(st.Properties.VariableNames)
        error('Nelson:table:varIndexOutOfRange', _('Variable index exceeds table dimensions.'));
      end
    end

    function value = sliceRows(obj, value, rows)
      if islogical(rows)
        rows = find(rows);
      end
      if isvector(value)
        value = value(rows, :);
      else
        idx = repmat({':'}, 1, ndims(value) - 1);
        value = value(rows, idx{:});
      end
    end

    function n = tableHeight(obj, st)
      if isempty(st.Properties.VariableNames)
        n = table.heightFromStruct(st);
      else
        n = variableHeight(obj, st.data.(st.Properties.VariableNames{1}));
      end
    end

    function n = variableHeight(obj, value)
      n = size(value, 1);
    end

    function tf = tableDataEqual(obj, a, b)
      names = a.Properties.VariableNames;
      if ~builtin('isequal', names, b.Properties.VariableNames)
        tf = false;
        return
      end
      tf = ~isempty(names) || table.heightFromStruct(a) == table.heightFromStruct(b);
      for k = 1:length(names)
        if ~variableEqual(obj, a.data.(names{k}), b.data.(names{k}))
          tf = false;
          return
        end
      end
    end

    function tf = variableEqual(obj, a, b)
      if isa(a, 'datetime') || isa(b, 'datetime')
        if ~(isa(a, 'datetime') && isa(b, 'datetime'))
          tf = false;
        else
          tf = builtin('isequal', datenum(a), datenum(b));
        end
      elseif isa(a, 'duration') || isa(b, 'duration')
        if ~(isa(a, 'duration') && isa(b, 'duration'))
          tf = false;
        else
          tf = builtin('isequal', seconds(a), seconds(b));
        end
      elseif isnumeric(a) && isnumeric(b)
        tf = numericVariableEqual(obj, a, b);
      else
        tf = builtin('isequal', a, b);
      end
    end

    function tf = numericVariableEqual(obj, a, b)
      if ndims(a) ~= ndims(b) || any(size(a) ~= size(b))
        tf = false;
        return
      end
      tf = builtin('isequal', a, b);
      if tf
        return
      end
      mask = (a == b) | (isnan(a) & isnan(b));
      tf = all(mask(:));
    end

    function names = toCellstrColumn(obj, names)
      if isempty(names)
        names = {};
        return
      end
      if isstring(names)
        names = cellstr(names);
      elseif ischar(names)
        names = {names};
      end
      names = names(:);
    end

    function s = formatCell(obj, value, row)
      try
        if isnumeric(value) || islogical(value)
          s = num2str(value(row, 1));
        elseif isstring(value)
          s = char(value(row));
        elseif iscell(value)
          element = value{row};
          if ischar(element)
            s = element;
          elseif isnumeric(element) || islogical(element)
            s = num2str(element);
          else
            s = ['[', class(element), ']'];
          end
        elseif ischar(value)
          s = value(row, :);
        else
          s = ['[', class(value), ']'];
        end
      catch
        s = '?';
      end
    end
  end
end
%=============================================================================
