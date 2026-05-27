%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef table
  properties (Hidden)
    data
    Version
  end

  properties
    Properties
  end

  methods
    function obj = table(varargin)
      obj.data = struct();
      obj.Version = 2;
      obj.Properties = defaultProperties(obj);
      if nargin == 0
        return
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

      if ~isempty(opts.Size) || ~isempty(opts.VariableTypes)
        obj = preallocate(obj, opts);
        return
      end

      obj = addConstructorData(obj, dataArgs, dataNames, ~isempty(opts.VariableNames));
      if ~isempty(opts.VariableNames)
        obj = renameAllVariables(obj, opts.VariableNames);
      end
      obj = applyConstructorProperties(obj, opts);
      obj = validateAndRefresh(obj);
    end

    function R = subsref(obj, sref)
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
          error(_('Unsupported subsref type.'));
      end
    end

    function obj = subsasgn(obj, sasgn, value)
      st = struct(obj);
      switch sasgn(1).type
        case '.'
          obj = dotSubsasgn(obj, st, sasgn, value);
        case '{}'
          obj = braceSubsasgn(obj, st, sasgn, value);
        case '()'
          obj = parenSubsasgn(obj, st, sasgn, value);
        otherwise
          error(_('Unsupported subsasgn type.'));
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

    function n = numel(varargin)
      n = 1;
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
      tf = tableHeight(obj, struct(obj)) == 0;
    end

    function names = properties(obj)
      st = struct(obj);
      names = [st.Properties.VariableNames(:); {'Properties'}; st.Properties.DimensionNames(:)];
    end

    function disp(obj)
      if isempty(obj)
        return
      end
      numRows = height(obj);
      NB_LINES_TO_DISPLAY = 20;
      if numRows > NB_LINES_TO_DISPLAY
        table.dispTableCompact(obj);
      else
        table.dispTableFull(obj);
      end
      currentFormat = format();
      if strcmp(currentFormat.LineSpacing, 'loose')
        builtin('disp', ' ');
      end
    end

    function display(obj, varargin)
      if nargin == 2
        name = varargin{1};
      else
        name = inputname(1);
      end
      currentFormat = format();
      if ~isempty(name)
        if strcmp(currentFormat.LineSpacing, 'loose')
          builtin('disp', ' ');
        end
        builtin('disp', [name, ' =']);
      end
      if strcmp(currentFormat.LineSpacing, 'loose')
        builtin('disp', ' ');
      end
      header = sprintf('  %d%s%d %s', height(obj), char(215), width(obj), 'table');
      builtin('disp', header);
      if strcmp(currentFormat.LineSpacing, 'loose')
        builtin('disp', ' ');
      end
      disp(obj);
    end

    function R = horzcat(varargin)
      R = table();
      for k = 1:nargin
        if ~istable(varargin{k})
          error(_('All inputs must be tables.'));
        end
        st = struct(varargin{k});
        if k > 1 && height(R) ~= height(varargin{k})
          error(_('All tables must have the same number of rows.'));
        end
        for j = 1:length(st.Properties.VariableNames)
          name = st.Properties.VariableNames{j};
          if any(strcmp(R.Properties.VariableNames, name))
            error(_('Duplicate table variable name.'));
          end
          R = addVariable(R, name, st.data.(name));
        end
        if k == 1
          R.Properties.RowNames = st.Properties.RowNames;
        end
      end
      R = validateAndRefresh(R);
    end

    function R = vertcat(varargin)
      if nargin == 0
        R = table();
        return
      end
      R = varargin{1};
      for k = 2:nargin
        if ~istable(R) || ~istable(varargin{k})
          error(_('All inputs must be tables.'));
        end
        if isempty(R)
          R = varargin{k};
          continue
        end
        if isempty(varargin{k})
          continue
        end
        stR = struct(R);
        stB = struct(varargin{k});
        if ~isequal(stR.Properties.VariableNames, stB.Properties.VariableNames)
          error(_('All tables to be vertically concatenated must have identical variable names.'));
        end
        for j = 1:length(stR.Properties.VariableNames)
          name = stR.Properties.VariableNames{j};
          stR.data.(name) = [stR.data.(name); stB.data.(name)];
        end
        stR.Properties.RowNames = [stR.Properties.RowNames(:); stB.Properties.RowNames(:)]';
        R = table.fromStruct(stR);
      end
    end

    function tf = isequal(obj, varargin)
      tf = true;
      if nargin < 2
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
            || ~builtin('isequal', first.data, current.data)
          tf = false;
          return
        end
      end
    end

    function tf = isequalto(obj, other)
      if ~istable(other)
        tf = false;
        return
      end
      tf = builtin('isequalto', struct(validateAndRefresh(obj)), struct(validateAndRefresh(other)));
    end

    function R = sortrows(obj, vars, direction)
      if nargin < 2 || isempty(vars)
        vars = 1:width(obj);
      end
      if nargin < 3
        direction = 'ascend';
      end
      st = struct(obj);
      idx = table.resolveVarIndices(st, vars);
      if length(idx) == 1 && (isnumeric(st.data.(st.Properties.VariableNames{idx})) || islogical(st.data.(st.Properties.VariableNames{idx})))
        sortData = st.data.(st.Properties.VariableNames{idx});
        [~, order] = sort(sortData(:, 1));
      else
        keys = table.rowKeysFromStruct(st, height(obj), idx);
        order = 1:length(keys);
      end
      if ischar(direction) && strcmpi(direction, 'descend')
        order = fliplr(order(:)');
      end
      R = obj(order, :);
    end

    function [C, ia, ic] = unique(obj, varargin)
      st = struct(obj);
      keys = table.rowKeysFromStruct(st, height(obj), 1:length(st.Properties.VariableNames));
      [~, ia, ic] = table.uniqueKeysStable(keys);
      C = obj(ia, :);
    end

    function [tf, loc] = ismember(A, B, varargin)
      if ~istable(B)
        error(_('Second input must be a table.'));
      end
      stA = struct(A);
      stB = struct(B);
      [tf, loc] = table.ismemberKeys(table.rowKeysFromStruct(stA, height(A), 1:length(stA.Properties.VariableNames)), ...
        table.rowKeysFromStruct(stB, height(B), 1:length(stB.Properties.VariableNames)));
    end

    function [C, ia, ib] = intersect(A, B, varargin)
      if ~istable(B)
        error(_('Second input must be a table.'));
      end
      stA = struct(A);
      stB = struct(B);
      [ia, ib] = table.intersectKeys(table.rowKeysFromStruct(stA, height(A), 1:length(stA.Properties.VariableNames)), ...
        table.rowKeysFromStruct(stB, height(B), 1:length(stB.Properties.VariableNames)));
      C = A(ia, :);
    end

    function [C, ia] = setdiff(A, B, varargin)
      if ~istable(B)
        error(_('Second input must be a table.'));
      end
      stA = struct(A);
      stB = struct(B);
      ia = table.setdiffKeys(table.rowKeysFromStruct(stA, height(A), 1:length(stA.Properties.VariableNames)), ...
        table.rowKeysFromStruct(stB, height(B), 1:length(stB.Properties.VariableNames)));
      C = A(ia, :);
    end

    function [C, ia, ib] = union(A, B, varargin)
      if ~istable(B)
        error(_('Second input must be a table.'));
      end
      combined = [A; B];
      stCombined = struct(combined);
      [~, selected] = table.uniqueKeysStable(table.rowKeysFromStruct(stCombined, height(combined), 1:length(stCombined.Properties.VariableNames)));
      C = combined(selected, :);
      ia = selected(selected <= height(A));
      ib = selected(selected > height(A)) - height(A);
    end

    function [C, ia, ib] = setxor(A, B, varargin)
      if ~istable(B)
        error(_('Second input must be a table.'));
      end
      stA = struct(A);
      stB = struct(B);
      keysA = table.rowKeysFromStruct(stA, height(A), 1:length(stA.Properties.VariableNames));
      keysB = table.rowKeysFromStruct(stB, height(B), 1:length(stB.Properties.VariableNames));
      ia = table.setdiffKeys(keysA, keysB);
      ib = table.setdiffKeys(keysB, keysA);
      C = [A(ia, :); B(ib, :)];
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

    function R = not(A)
      R = table.applyUnary(A, @not);
    end

    function R = abs(A)
      R = table.applyUnary(A, @abs);
    end

    function R = ceil(A)
      R = table.applyUnary(A, @ceil);
    end

    function R = fix(A)
      R = table.applyUnary(A, @fix);
    end

    function R = floor(A)
      R = table.applyUnary(A, @floor);
    end

    function R = round(A)
      R = table.applyUnary(A, @round);
    end

    function R = sqrt(A)
      R = table.applyUnary(A, @sqrt);
    end

    function R = exp(A)
      R = table.applyUnary(A, @exp);
    end

    function R = log(A)
      R = table.applyUnary(A, @log);
    end

    function R = log10(A)
      R = table.applyUnary(A, @log10);
    end

    function R = sin(A)
      R = table.applyUnary(A, @sin);
    end

    function R = cos(A)
      R = table.applyUnary(A, @cos);
    end

    function R = tan(A)
      R = table.applyUnary(A, @tan);
    end

    function R = acos(A)
      R = table.applyUnary(A, @acos);
    end
    function R = acosd(A)
      R = table.applyUnary(A, @acosd);
    end
    function R = acosh(A)
      R = table.applyUnary(A, @acosh);
    end
    function R = acot(A)
      R = table.applyUnary(A, @acot);
    end
    function R = acotd(A)
      R = table.applyUnary(A, @acotd);
    end
    function R = acoth(A)
      R = table.applyUnary(A, @acoth);
    end
    function R = acsc(A)
      R = table.applyUnary(A, @acsc);
    end
    function R = acscd(A)
      R = table.applyUnary(A, @acscd);
    end
    function R = acsch(A)
      R = table.applyUnary(A, @acsch);
    end
    function R = asec(A)
      R = table.applyUnary(A, @asec);
    end
    function R = asecd(A)
      R = table.applyUnary(A, @asecd);
    end
    function R = asech(A)
      R = table.applyUnary(A, @asech);
    end
    function R = asin(A)
      R = table.applyUnary(A, @asin);
    end
    function R = asind(A)
      R = table.applyUnary(A, @asind);
    end
    function R = asinh(A)
      R = table.applyUnary(A, @asinh);
    end
    function R = atan(A)
      R = table.applyUnary(A, @atan);
    end
    function R = atand(A)
      R = table.applyUnary(A, @atand);
    end
    function R = atanh(A)
      R = table.applyUnary(A, @atanh);
    end
    function R = cosd(A)
      R = table.applyUnary(A, @cosd);
    end
    function R = cosh(A)
      R = table.applyUnary(A, @cosh);
    end
    function R = cospi(A)
      R = table.applyUnary(A, @cospi);
    end
    function R = cot(A)
      R = table.applyUnary(A, @cot);
    end
    function R = cotd(A)
      R = table.applyUnary(A, @cotd);
    end
    function R = coth(A)
      R = table.applyUnary(A, @coth);
    end
    function R = csc(A)
      R = table.applyUnary(A, @csc);
    end
    function R = cscd(A)
      R = table.applyUnary(A, @cscd);
    end
    function R = csch(A)
      R = table.applyUnary(A, @csch);
    end
    function R = log1p(A)
      R = table.applyUnary(A, @log1p);
    end
    function R = log2(A)
      R = table.applyUnary(A, @log2);
    end
    function R = nextpow2(A)
      R = table.applyUnary(A, @nextpow2);
    end
    function R = sec(A)
      R = table.applyUnary(A, @sec);
    end
    function R = secd(A)
      R = table.applyUnary(A, @secd);
    end
    function R = sech(A)
      R = table.applyUnary(A, @sech);
    end
    function R = sind(A)
      R = table.applyUnary(A, @sind);
    end
    function R = sinh(A)
      R = table.applyUnary(A, @sinh);
    end
    function R = sinpi(A)
      R = table.applyUnary(A, @sinpi);
    end
    function R = tand(A)
      R = table.applyUnary(A, @tand);
    end
    function R = tanh(A)
      R = table.applyUnary(A, @tanh);
    end
    function R = var(A)
      R = table.applyUnary(A, @var);
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
    function obj = fromStruct(st)
      obj = table();
      if isfield(st, 'data')
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

      headPart = T(1:NB_LINES_BEFORE, :);
      tailPart = T(numRows - NB_LINES_AFTER + 1:numRows, :);

      nbColsToDisplay = numCols + 1;
      nbRowsToDisplay = NB_LINES_BEFORE + NB_LINES_AFTER + NB_LINES_ELLIPSIS + 1;
      strs = string(cell(nbRowsToDisplay, nbColsToDisplay));
      for j = 1:numCols
        strs{1, j + 1} = varNames{j};
      end

      for j = 1:numCols
        for i = 1:NB_LINES_BEFORE
          strs{i + 1, j + 1} = table.displayCellText(headPart{i, j});
        end
      end

      for j = 1:numCols
        for i = NB_LINES_BEFORE + 1:NB_LINES_BEFORE + NB_LINES_ELLIPSIS
          strs{i + 1, j + 1} = ':';
        end
      end

      for j = 1:numCols
        for i = 1:NB_LINES_AFTER
          idx = NB_LINES_BEFORE + NB_LINES_ELLIPSIS + i + 1;
          strs{idx, j + 1} = table.displayCellText(tailPart{i, j});
        end
      end

      table.formatTableDisplay(strs, haveRowsNames);
    end

    function dispTableFull(T)
      st = struct(T);
      varNames = st.Properties.VariableNames;
      numCols = width(T);
      numRows = height(T);
      haveRowsNames = ~isempty(st.Properties.RowNames);
      strs = string(cell(numRows + 1, numCols + 1));

      for j = 1:numCols
        for i = 1:numRows
          strs{i + 1, j + 1} = table.displayCellText(T{i, j});
        end
      end

      for j = 1:numCols
        strs{1, j + 1} = varNames{j};
      end

      if haveRowsNames
        for i = 1:numRows
          strs{i + 1, 1} = st.Properties.RowNames{i};
        end
      end

      table.formatTableDisplay(strs, haveRowsNames);
    end

    function value = displayCellText(value)
      originalValue = value;
      is2d = length(size(value));
      if is2d < 3
        isString = isa(value, 'string');
        value = formattedDisplayText(value);
        if isString
          value = """" + value + """";
        end
        value = char(value);
        value = strtrim(value(1:end));
        if length(value) > 15
          value = table.sizeAsString(originalValue);
        end
      else
        value = table.sizeAsString(value);
      end
    end

    function s = sizeAsString(value)
      sz = size(value);
      sizeStr = sprintf('%dx', sz(1:end));
      sizeStr = sizeStr(1:end - 1);
      s = [sizeStr, ' ', class(value)];
    end

    function formatTableDisplay(strs, haveRowsNames)
      numCols = size(strs, 2);
      newRow = strings(1, numCols);
      strs = [strs(1, :); newRow; newRow; strs(2:end, :)];

      maxLenPerCol = max(strlength(strs), [], 1);
      startIndex = 1;
      if haveRowsNames
        startIndex = 2;
      end

      for j = startIndex:numCols
        strs(2, j) = repmat('_', 1, maxLenPerCol(j));
      end

      B = strings(size(strs));
      for j = 1:size(strs, 2)
        for i = 1:size(strs, 1)
          nbBlanks = maxLenPerCol(j) - strlength(strs(i, j));
          B(i, j) = strjust([blanks(nbBlanks), strs{i, j}], 'left');
        end
      end

      blanksSeparator = '    ';
      startIndex = 2;
      if haveRowsNames
        startIndex = 1;
      end
      for i = 1:size(B, 1)
        line = blanksSeparator + join(B(i, startIndex:end), blanksSeparator) + blanksSeparator;
        builtin('disp', line);
      end
    end

    function R = applyUnary(A, fun)
      if ~istable(A)
        error(_('Input must be a table.'));
      end
      st = struct(A);
      R = A;
      for k = 1:length(st.Properties.VariableNames)
        name = st.Properties.VariableNames{k};
        try
          newValue = fun(st.data.(name));
        catch
          table.rethrowUnaryFailure(fun, st.data.(name));
        end
        if size(newValue, 1) ~= size(st.data.(name), 1)
          table.rethrowUnaryFailure(fun, st.data.(name));
        end
        R.data.(name) = newValue;
      end
      R.Properties.VariableTypes = string([]);
      R = validateAndRefresh(R);
    end

    function R = applyBinary(A, B, fun)
      if istable(A) && istable(B)
        stA = struct(A);
        stB = struct(B);
        if ~isequal(stA.Properties.VariableNames, stB.Properties.VariableNames)
          error(_('Table variable names must match.'));
        end
        R = A;
        for k = 1:length(stA.Properties.VariableNames)
          name = stA.Properties.VariableNames{k};
          try
            newValue = fun(stA.data.(name), stB.data.(name));
          catch
            table.rethrowBinaryFailure(fun, stA.data.(name));
          end
          if size(newValue, 1) ~= size(stA.data.(name), 1)
            table.rethrowBinaryFailure(fun, stA.data.(name));
          end
          R.data.(name) = newValue;
        end
      elseif istable(A)
        stA = struct(A);
        R = A;
        for k = 1:length(stA.Properties.VariableNames)
          name = stA.Properties.VariableNames{k};
          try
            newValue = fun(stA.data.(name), B);
          catch
            table.rethrowBinaryFailure(fun, stA.data.(name));
          end
          if size(newValue, 1) ~= size(stA.data.(name), 1)
            table.rethrowBinaryFailure(fun, stA.data.(name));
          end
          R.data.(name) = newValue;
        end
      elseif istable(B)
        stB = struct(B);
        R = B;
        for k = 1:length(stB.Properties.VariableNames)
          name = stB.Properties.VariableNames{k};
          try
            newValue = fun(A, stB.data.(name));
          catch
            table.rethrowBinaryFailure(fun, stB.data.(name));
          end
          if size(newValue, 1) ~= size(stB.data.(name), 1)
            table.rethrowBinaryFailure(fun, stB.data.(name));
          end
          R.data.(name) = newValue;
        end
      else
        R = fun(A, B);
        return
      end
      R.Properties.VariableTypes = string([]);
      R = validateAndRefresh(R);
    end

    function rethrowBinaryFailure(fun, value)
      functionName = func2str(fun);
      className = class(value);
      msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), functionName, className);
      ME = MException('Nelson:table:math:FunFailed', msg);
      throwAsCaller(ME);
    end

    function rethrowUnaryFailure(fun, value)
      functionName = func2str(fun);
      className = class(value);
      msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), functionName, className);
      ME = MException('Nelson:table:math:FunFailed', msg);
      throwAsCaller(ME);
    end
  end

  methods (Static, Access = private)
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
          error(_('Unrecognized table variable name.'));
        end
        idx(k) = found;
      end
    end

    function keys = rowKeysFromStruct(st, nRows, vars)
      if nargin < 3 || isempty(vars)
        vars = 1:length(st.Properties.VariableNames);
      end
      keys = cell(nRows, 1);
      for r = 1:nRows
        key = '';
        for j = 1:length(vars)
          name = st.Properties.VariableNames{vars(j)};
          column = st.data.(name);
          key = [key, '#', int2str(j), '=', table.valueKey(table.columnRows(column, r)), ';'];
        end
        keys{r} = key;
      end
    end

    function value = columnRows(value, rows)
      if isvector(value)
        value = value(rows, :);
      else
        idx = repmat({':'}, 1, ndims(value) - 1);
        value = value(rows, idx{:});
      end
    end

    function key = valueKey(value)
      if ischar(value)
        key = ['char:', value];
      elseif isstring(value)
        key = ['string:', char(value)];
      elseif iscell(value)
        if isempty(value)
          key = 'cell:[]';
        else
          key = ['cell:', table.valueKey(value{1})];
        end
      elseif isnumeric(value) || islogical(value)
        key = [class(value), ':', mat2str(value)];
      else
        try
          key = [class(value), ':', mat2str(value)];
        catch
          key = [class(value), ':', char(string(value))];
        end
      end
    end

    function [values, ia, ic] = uniqueKeysStable(valuesIn)
      values = {};
      ia = [];
      ic = zeros(size(valuesIn));
      for k = 1:length(valuesIn)
        found = find(strcmp(values, valuesIn{k}), 1);
        if isempty(found)
          values{end + 1} = valuesIn{k};
          ia(end + 1) = k;
          found = length(values);
        end
        ic(k) = found;
      end
      values = values(:);
      ia = ia(:);
    end

    function [tf, loc] = ismemberKeys(keysA, keysB)
      tf = false(length(keysA), 1);
      loc = zeros(length(keysA), 1);
      for k = 1:length(keysA)
        found = find(strcmp(keysB, keysA{k}), 1);
        if ~isempty(found)
          tf(k) = true;
          loc(k) = found;
        end
      end
    end

    function [ia, ib] = intersectKeys(keysA, keysB)
      ia = [];
      ib = [];
      used = {};
      for k = 1:length(keysA)
        found = find(strcmp(keysB, keysA{k}), 1);
        if ~isempty(found) && ~any(strcmp(used, keysA{k}))
          ia(end + 1) = k;
          ib(end + 1) = found;
          used{end + 1} = keysA{k};
        end
      end
      ia = ia(:);
      ib = ib(:);
    end

    function ia = setdiffKeys(keysA, keysB)
      ia = [];
      used = {};
      for k = 1:length(keysA)
        if isempty(find(strcmp(keysB, keysA{k}), 1)) && ~any(strcmp(used, keysA{k}))
          ia(end + 1) = k;
          used{end + 1} = keysA{k};
        end
      end
      ia = ia(:);
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
      props.RowNames = toCellstrRow(obj, props.RowNames);
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
      result.dataArgs = {};
      result.dataNames = {};
      k = 1;
      while k <= length(args)
        option = isConstructorOption(obj, args{k});
        if option.isOption
          if k == length(args)
            error(_('Name-value argument must be followed by a value.'));
          end
          result.opts.(option.name) = args{k + 1};
          k = k + 2;
        else
          result.dataArgs{end + 1} = args{k};
          if k <= length(argNames)
            result.dataNames{end + 1} = argNames{k};
          else
            result.dataNames{end + 1} = '';
          end
          k = k + 1;
        end
      end
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

    function obj = addConstructorData(obj, dataArgs, dataNames, hasExplicitVariableNames)
      namePrefix = 'Var';
      if ~isempty(dataNames) && ~isempty(dataNames{1})
        namePrefix = dataNames{1};
      end
      for j = 1:length(dataArgs)
        value = normalizeColumnValue(obj, dataArgs{j});
        if isempty(dataNames{j})
          name = [namePrefix, num2str(j)];
        else
          name = dataNames{j};
        end
        if iscell(value) && ~isempty(value) && size(value, 2) > 1 && hasExplicitVariableNames
          for k = 1:size(value, 2)
            obj = addVariable(obj, ['Var', num2str(length(obj.Properties.VariableNames) + 1)], normalizeCellColumn(obj, value(:, k)));
          end
        else
          obj = addVariable(obj, name, value);
        end
      end
    end

    function obj = addVariable(obj, name, value)
      obj.Properties.VariableNames{end + 1} = name;
      obj.data.(name) = value;
    end

    function obj = renameAllVariables(obj, newVariableNames)
      newVariableNames = toCellstrRow(obj, newVariableNames);
      if length(newVariableNames) ~= length(obj.Properties.VariableNames)
        error(_('Mismatched variable names.'));
      end
      validateNames(obj, newVariableNames, _('Variable names must be nonempty and distinct.'));
      newData = struct();
      for k = 1:length(newVariableNames)
        oldName = obj.Properties.VariableNames{k};
        newData.(newVariableNames{k}) = obj.data.(oldName);
      end
      obj.data = newData;
      obj.Properties.VariableNames = newVariableNames;
    end

    function obj = applyConstructorProperties(obj, opts)
      if ~isempty(opts.RowNames)
        obj.Properties.RowNames = toCellstrRow(obj, opts.RowNames);
      end
      if ~isempty(opts.DimensionNames)
        obj.Properties.DimensionNames = toCellstrRow(obj, opts.DimensionNames);
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
      if isempty(opts.Size) || isempty(opts.VariableTypes)
        error(_('Size and VariableTypes must be specified together.'));
      end
      if ~isnumeric(opts.Size) || length(opts.Size) ~= 2
        error(_('Size must be a two-element numeric vector.'));
      end
      nRows = opts.Size(1);
      nVars = opts.Size(2);
      variableTypes = toCellstrRow(obj, opts.VariableTypes);
      if length(variableTypes) ~= nVars
        error(_('VariableTypes must match the number of variables.'));
      end
      if isempty(opts.VariableNames)
        variableNames = cell(1, nVars);
        for k = 1:nVars
          variableNames{k} = ['Var', num2str(k)];
        end
      else
        variableNames = toCellstrRow(obj, opts.VariableNames);
      end
      if length(variableNames) ~= nVars
        error(_('VariableNames must match the number of variables.'));
      end
      obj.data = struct();
      obj.Properties = defaultProperties(obj);
      for k = 1:nVars
        obj = addVariable(obj, variableNames{k}, initialValue(obj, nRows, variableTypes{k}));
      end
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
          error(sprintf(_('Unsupported variable type: %s'), typeName));
      end
    end

    function obj = validateAndRefresh(obj)
      obj.Properties = completeProperties(obj, obj.Properties);
      validateNames(obj, obj.Properties.VariableNames, _('Variable names must be nonempty and distinct.'));
      if length(obj.Properties.DimensionNames) ~= 2
        error(_('DimensionNames must contain two names.'));
      end
      validateNames(obj, obj.Properties.DimensionNames, _('Dimension names must be nonempty and distinct.'));
      dataNames = fieldnames(obj.data);
      names = obj.Properties.VariableNames;
      if length(dataNames) == length(names) && ~isempty(names)
        missingNames = false;
        for k = 1:length(names)
          if ~isfield(obj.data, names{k})
            missingNames = true;
          end
        end
        if missingNames
          newData = struct();
          for k = 1:length(names)
            newData.(names{k}) = obj.data.(dataNames{k});
          end
          obj.data = newData;
          dataNames = fieldnames(obj.data);
        end
      end
      if length(dataNames) > length(names)
        for k = 1:length(dataNames)
          if ~any(strcmp(names, dataNames{k}))
            names{end + 1} = dataNames{k};
          end
        end
        obj.Properties.VariableNames = names;
      end
      if ~isempty(names)
        nRows = size(obj.data.(names{1}), 1);
        for k = 2:length(names)
          if ~isequal(nRows, size(obj.data.(names{k}), 1))
            error(_('All table variables must have the same number of rows.'));
          end
        end
        if ~isempty(obj.Properties.RowNames) && length(obj.Properties.RowNames) ~= nRows
          error(_('RowNames must match the number of rows.'));
        end
      elseif ~isempty(obj.Properties.RowNames)
        error(_('RowNames must match the number of rows.'));
      end
      requestedTypes = obj.Properties.VariableTypes;
      if length(requestedTypes) == length(names)
        for k = 1:length(names)
          requestedType = char(requestedTypes(k));
          if ~isempty(requestedType) && ~strcmp(requestedType, class(obj.data.(names{k})))
            obj.data.(names{k}) = castVariable(obj, obj.data.(names{k}), requestedType);
          end
        end
      end
      obj.Properties.VariableTypes = variableTypes(obj);
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
          error(sprintf(_('Unsupported variable type: %s'), typeName));
      end
    end

    function types = variableTypes(obj)
      names = obj.Properties.VariableNames;
      types = string([]);
      for k = 1:length(names)
        types(end + 1) = class(obj.data.(names{k}));
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
      else
        error(_('Unrecognized table variable name.'));
      end
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function R = braceSubsref(obj, st, sref)
      if length(sref(1).subs) ~= 2
        error(_('Brace indexing requires row and variable subscripts.'));
      end
      rows = resolveRows(obj, st, sref(1).subs{1});
      vars = resolveVars(obj, st, sref(1).subs{2});
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
        error(_('Parentheses indexing requires row and variable subscripts.'));
      end
      rows = resolveRows(obj, st, sref(1).subs{1});
      vars = resolveVars(obj, st, sref(1).subs{2});
      R = table();
      for k = 1:length(vars)
        name = st.Properties.VariableNames{vars(k)};
        R = addVariable(R, name, sliceRows(obj, st.data.(name), rows));
      end
      if ~isempty(st.Properties.RowNames)
        R.Properties.RowNames = st.Properties.RowNames(rows);
      end
      R.Properties.DimensionNames = st.Properties.DimensionNames;
      R.Properties.Description = st.Properties.Description;
      R.Properties.UserData = st.Properties.UserData;
      R = validateAndRefresh(R);
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function obj = dotSubsasgn(obj, st, sasgn, value)
      if length(sasgn) ~= 1
        error(_('Unsupported table assignment.'));
      end
      field = sasgn(1).subs;
      if strcmp(field, 'Properties')
        st.Properties = completeProperties(obj, value);
        obj = table.fromStruct(st);
        return
      end
      if isempty(value) && isequal(size(value), [0 0])
        idx = find(strcmp(st.Properties.VariableNames, field));
        if isempty(idx)
          error(_('Unrecognized table variable name.'));
        end
        st.data = rmfield(st.data, field);
        st.Properties.VariableNames(idx) = [];
        st.Properties.VariableTypes(idx) = [];
        obj = table.fromStruct(st);
        obj.Properties.VariableNames = obj.Properties.VariableNames(:);
        return
      end
      if isempty(st.Properties.VariableNames)
        nRows = size(value, 1);
      else
        nRows = tableHeight(obj, st);
      end
      if any(strcmp(st.Properties.VariableNames, field)) && ~isequal(size(value), size(st.data.(field)))
        error(_('Value assignment must be same size as existing value.'));
      end
      if size(value, 1) ~= nRows
        error(_('Value assignment must have the same number of rows as the table.'));
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
        error(_('Brace indexing requires row and variable subscripts.'));
      end
      rows = resolveRows(obj, st, sasgn(1).subs{1});
      vars = resolveVars(obj, st, sasgn(1).subs{2});
      if isempty(value) && isequal(size(value), [0 0])
        error(_('To delete rows or variables by assigning [], use () subscripting instead of {}.'));
      end
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
          error(_('Value assignment must be same size as existing value.'));
        end
        st.data.(name) = col;
      end
      st.Properties.VariableTypes = string([]);
      obj = table.fromStruct(st);
    end

    function obj = parenSubsasgn(obj, st, sasgn, value)
      if length(sasgn(1).subs) ~= 2
        error(_('Parentheses indexing requires row and variable subscripts.'));
      end
      rows = resolveRows(obj, st, sasgn(1).subs{1});
      vars = resolveVars(obj, st, sasgn(1).subs{2});
      if isempty(value) && isequal(size(value), [0 0])
        if ischar(sasgn(1).subs{1}) && strcmp(sasgn(1).subs{1}, ':')
          for k = fliplr(vars)
            name = st.Properties.VariableNames{k};
            st.data = rmfield(st.data, name);
          end
          st.Properties.VariableNames(vars) = [];
          st.Properties.VariableTypes(vars) = [];
        elseif ischar(sasgn(1).subs{2}) && strcmp(sasgn(1).subs{2}, ':')
          keep = true(tableHeight(obj, st), 1);
          keep(rows) = false;
          for k = 1:length(st.Properties.VariableNames)
            name = st.Properties.VariableNames{k};
            st.data.(name) = sliceRows(obj, st.data.(name), keep);
          end
          if ~isempty(st.Properties.RowNames)
            st.Properties.RowNames = st.Properties.RowNames(keep);
          end
        else
          error(_('At least one subscript must be '':'' when you delete rows or variables by assigning [].'));
        end
        obj = table.fromStruct(st);
        return
      end
      if ~istable(value)
        error(_('Parentheses assignment value must be a table.'));
      end
      valueSt = struct(value);
      if length(vars) ~= length(valueSt.Properties.VariableNames) || length(rows) ~= height(value)
        error(_('Assigned table must match the indexed table size.'));
      end
      for k = 1:length(vars)
        name = st.Properties.VariableNames{vars(k)};
        sourceName = valueSt.Properties.VariableNames{k};
        col = st.data.(name);
        col(rows, :) = valueSt.data.(sourceName);
        st.data.(name) = col;
      end
      st.Properties.VariableTypes = string([]);
      obj = table.fromStruct(st);
    end

    function rows = resolveRows(obj, st, sub)
      if ischar(sub) && strcmp(sub, ':')
        rows = 1:tableHeight(obj, st);
      elseif islogical(sub)
        rows = find(sub);
      elseif isnumeric(sub)
        rows = sub;
      elseif ischar(sub) || isstring(sub) || iscellstr(sub)
        names = toCellstrRow(obj, sub);
        implicitNames = {};
        if isempty(st.Properties.RowNames)
          implicitNames = firstVariableRowNames(obj, st);
        end
        rows = zeros(1, length(names));
        for k = 1:length(names)
          idx = find(strcmp(st.Properties.RowNames, names{k}));
          if isempty(idx) && ~isempty(implicitNames)
            idx = find(strcmp(implicitNames, names{k}));
          end
          if isempty(idx)
            error(_('Row name not found.'));
          end
          rows(k) = idx(1);
        end
      else
        error(_('Invalid row subscript type.'));
      end
    end

    function names = firstVariableRowNames(obj, st)
      names = {};
      if isempty(st.Properties.VariableNames)
        return
      end
      value = st.data.(st.Properties.VariableNames{1});
      if isstring(value) && isvector(value)
        names = cellstr(value(:))';
      elseif iscellstr(value)
        names = value(:)';
      elseif ischar(value) && size(value, 1) == tableHeight(obj, st)
        names = cellstr(value)';
      end
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
            error(_('Unrecognized table variable name.'));
          end
          vars(k) = idx(1);
        end
      else
        error(_('Invalid variable subscript type.'));
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
        n = 0;
      else
        n = size(st.data.(st.Properties.VariableNames{1}), 1);
      end
    end

    function value = normalizeColumnValue(obj, value)
      if iscell(value) && ~isempty(value) && iscolumn(value)
        value = normalizeCellColumn(obj, value);
      end
    end

    function value = normalizeCellColumn(obj, value)
      if isempty(value)
        return
      end
      firstValue = value{1};
      if isnumeric(firstValue) || islogical(firstValue)
        try
          value = cell2mat(value);
        catch
        end
      elseif all(cellfun(@isstring, value))
        value = string(value);
      end
    end

    function names = toCellstrRow(obj, names)
      if isempty(names)
        names = {};
      elseif isstring(names)
        names = cellstr(names);
      elseif ischar(names)
        names = {names};
      end
      names = names(:)';
    end

    function validateNames(obj, names, message)
      if isempty(names)
        return
      end
      if ~iscellstr(names)
        error(message);
      end
      for k = 1:length(names)
        if isempty(names{k})
          error(message);
        end
      end
      if length(unique(names)) ~= length(names)
        error(message);
      end
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
