%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef timetable < tabular
  properties (Hidden)
    Table
    Version
  end

  properties
    Properties
  end

  methods
    function obj = timetable(varargin)
      obj.Table = timetableCreateTableStorage();
      obj.Version = 1;
      obj.Properties = defaultProperties(obj);
      if nargin == 0
        obj.Properties.RowTimes = seconds(zeros(0, 1));
        return
      end

      argNames = cell(1, nargin);
      for k = 1:nargin
        argNames{k} = inputname(k);
      end
      namedArgs = normalizeNamedArguments(obj, varargin, argNames);
      parsedArgs = parseConstructorArguments(obj, namedArgs.args, namedArgs.argNames);
      opts = parsedArgs.opts;
      dataArgs = parsedArgs.dataArgs;
      dataNames = parsedArgs.dataNames;

      if ~isempty(opts.Size) || ~isempty(opts.VariableTypes)
        obj = preallocate(obj, opts);
        return
      end

      [rowTimes, dataArgs, dataNames, rowTimeName] = resolveRowTimes(obj, dataArgs, dataNames, opts);
      T = timetableCreateTableStorage(dataArgs{:});
      if ~isempty(opts.VariableNames)
        T.Properties.VariableNames = checkedVariableNames(obj, opts.VariableNames);
      else
        for k = 1:length(dataNames)
          if ~isempty(dataNames{k})
            T.Properties.VariableNames{k} = dataNames{k};
          end
        end
      end
      obj.Table = T;
      if ~isempty(rowTimeName)
        obj.Properties.DimensionNames{1} = rowTimeName;
      end
      obj = applyConstructorDimensionNames(obj, opts);
      obj.Properties = mergeTableProperties(obj, obj.Properties, T.Properties);
      obj.Properties.RowTimes = columnTimes(obj, rowTimes);
      obj = applyConstructorProperties(obj, opts);
      obj = validateAndRefresh(obj);
    end

    function R = subsref(obj, sref)
      if ~(strcmp(sref(1).type, '.') && strcmp(sref(1).subs, 'Properties'))
        obj = validateAndRefresh(obj);
      end
      if strcmp(sref(1).type, '.') && ~ischar(sref(1).subs)
        R = subsref(tableObject(obj), [sref(1), timetableVariableRowSubscript(obj.Properties, sref(1).subs, sref(2:end))]);
        return
      end
      switch sref(1).type
        case '.'
          R = dotSubsref(obj, sref);
        case '{}'
          R = braceSubsref(obj, sref);
        case '()'
          R = parenSubsref(obj, sref);
        otherwise
          error('Nelson:timetable:unsupportedSubscriptType', 'Unsupported %s type.', 'subsref');
      end
    end

    function obj = subsasgn(obj, sasgn, value)
      if isscalar(sasgn) && strcmp(sasgn.type, '.') && strcmp(sasgn.subs, 'Properties') ...
          && isstruct(value) && isscalar(value) && all(isfield(value, {'Description', 'UserData'}))
        % Only Description and UserData differ: nothing to refresh.
        probe = obj.Properties;
        probe.Description = value.Description;
        probe.UserData = value.UserData;
        if isequal(probe, value) && strcmp(class(probe.Events), class(value.Events))
          obj.Properties = value;
          return
        end
      end
      if isscalar(sasgn) && strcmp(sasgn.type, '.') && strcmp(sasgn.subs, 'Properties') ...
          && isstruct(value) && isfield(value, 'RowTimes') && isTimeVector(obj, value.RowTimes) ...
          && timeLength(obj, value.RowTimes) ~= timeLength(obj, obj.Properties.RowTimes)
        error('Nelson:timetable:incorrectNumberOfRowTimes', _('RowTimes property must contain one time for each row in the timetable.'));
      end
      obj = validateAndRefresh(obj);
      if strcmp(sasgn(1).type, '.') && ~ischar(sasgn(1).subs)
        T = subsasgn(tableObject(obj), sasgn, value); % TT.(n), TT.(n)(indices)
        obj.Table = struct(T);
        obj.Properties.RowTimes = timetableGrowRowTimes(obj.Properties.RowTimes, height(T));
        obj = validateAndRefresh(obj);
        return
      end
      switch sasgn(1).type
        case '.'
          obj = dotSubsasgn(obj, sasgn, value);
        case '{}'
          [rows, newTimes] = timetableResolveRows(obj.Properties.RowTimes, sasgn(1).subs{1}, true);
          T = tableObject(obj);
          T{rows, sasgn(1).subs{2}} = value;
          obj.Table = struct(T);
          obj.Properties.RowTimes = timetableGrowRowTimes(obj.Properties.RowTimes, height(T), newTimes);
          obj = validateAndRefresh(obj);
        case '()'
          obj = parenSubsasgn(obj, sasgn, value);
        otherwise
          error('Nelson:timetable:unsupportedSubscriptType', 'Unsupported %s type.', 'subsasgn');
      end
    end

    function obj = set.Properties(obj, value)
      % The per-variable metadata lives in the table storage, which
      % validateAndRefresh copies into Properties. An edit of that metadata
      % on a synchronized timetable is pushed into the storage; while the
      % storage is ahead (internal structural update), Properties is stale
      % and is only stored.
      if isstruct(obj.Properties)
        if ~isstruct(value) || ~isequal(fieldnames(value), fieldnames(obj.Properties))
          value = checkPropertiesAssignment(obj, obj.Properties, value);
        end
        if ~isequal(value.DimensionNames, obj.Properties.DimensionNames)
          value.DimensionNames = checkedDimensionNames(obj, value.DimensionNames);
        end
        checkDimensionVariableNames(obj, value);
        if ~sameVariableProperties(obj, obj.Properties, value)
          checkReservedVariableNames(obj, value.VariableNames);
          st = tableStructValue(obj);
          if sameVariableProperties(obj, obj.Properties, st.Properties)
            if isfield(st, 'VariableCustomProperties')
              value = checkVariableCustomProperties(obj, value, obj.Properties, st.VariableCustomProperties);
            end
            [st.data, value] = applyVariableProperties(obj, st.data, st.Properties.VariableNames, value);
            obj.Table = timetableApplyVariableProperties(st, value);
          end
        end
      end
      obj.Properties = value;
    end

    function varargout = size(obj, varargin)
      sz = [timeLength(obj, obj.Properties.RowTimes), tableWidth(obj)];
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

    function n = end(obj, k, count)
      sz = size(obj);
      if count == 1
        n = prod(sz);
      elseif k <= length(sz)
        n = sz(k);
      else
        n = 1;
      end
    end

    function tf = isempty(obj)
      tf = height(obj) == 0 || tableWidth(obj) == 0;
    end

    function names = properties(obj)
      names = [obj.Properties.VariableNames(:); {'Properties'}; obj.Properties.DimensionNames(:)];
    end

    function disp(obj)
      obj = validateAndRefresh(obj);
      if width(obj) == 0 && height(obj) == 0
        return
      end
      dispTimetableFull(obj);
    end

    function R = horzcat(varargin)
      if nargin == 0
        R = timetable();
        return
      end
      if timetableHasEventtable(varargin)
        R = eventtable.applyTimetableFunction(@horzcat, varargin{:});
        return
      end
      R = varargin{1};
      if ~istimetable(R)
        error('Nelson:timetable:allInputsMustBe', 'All inputs must be %s.', 'timetables');
      end
      for k = 2:nargin
        if ~istimetable(varargin{k})
          error('Nelson:timetable:allInputsMustBe', 'All inputs must be %s.', 'timetables');
        end
        if height(R) ~= height(varargin{k}) || ~isequal(R.Properties.RowTimes, varargin{k}.Properties.RowTimes)
          error('Nelson:timetable:rowTimesMismatch', 'All timetables must have the same row times.');
        end
        R.Table = struct([tableObject(R), tableObject(varargin{k})]);
        R = validateAndRefresh(R);
      end
    end

    function R = vertcat(varargin)
      if nargin == 0
        R = timetable();
        return
      end
      if timetableHasEventtable(varargin)
        R = eventtable.applyTimetableFunction(@vertcat, varargin{:});
        return
      end
      R = varargin{1};
      if ~istimetable(R)
        error('Nelson:timetable:allInputsMustBe', 'All inputs must be %s.', 'timetables');
      end
      for k = 2:nargin
        if ~istimetable(varargin{k})
          error('Nelson:timetable:allInputsMustBe', 'All inputs must be %s.', 'timetables');
        end
        R.Table = struct([tableObject(R); tableObject(varargin{k})]);
        R.Properties.RowTimes = [R.Properties.RowTimes; varargin{k}.Properties.RowTimes];
        R = validateAndRefresh(R);
      end
    end

    function tf = isequal(obj, varargin)
      % Dispatched when any argument is a timetable, not only the first.
      tf = istimetable(obj);
      if ~tf
        return
      end
      first = validateAndRefresh(obj);
      for k = 1:length(varargin)
        if ~istimetable(varargin{k})
          tf = false;
          return
        end
        current = validateAndRefresh(varargin{k});
        firstStruct = struct(first);
        currentStruct = struct(current);
        if ~builtin('isequal', firstStruct.Table, currentStruct.Table) ...
            || ~builtin('isequal', first.Properties, current.Properties)
          tf = false;
          return
        end
      end
    end
  end

  methods (Static)
    obj = empty(varargin)

    function obj = fromTableAndTimes(T, rowTimes, props)
      obj = timetable();
      if istable(T)
        obj.Table = struct(T);
      else
        obj.Table = T;
      end
      if nargin >= 3 && ~isempty(props)
        obj.Properties = props;
      else
        obj.Properties = defaultProperties(obj);
      end
      tableStruct = tableStructValue(obj);
      obj.Properties = mergeTableProperties(obj, obj.Properties, tableStruct.Properties);
      obj.Properties.RowTimes = columnTimes(obj, rowTimes);
      obj = validateAndRefresh(obj);
    end
  end

  methods (Access = private)
    function props = defaultProperties(obj)
      props = struct();
      props.DimensionNames = {'Time', 'Variables'};
      props.Description = '';
      props.UserData = [];
      props.RowTimes = [];
      props.StartTime = [];
      props.SampleRate = [];
      props.TimeStep = [];
      props.Events = [];
      props.VariableNames = {};
      props.VariableTypes = string([]);
      props.VariableDescriptions = {};
      props.VariableUnits = {};
      props.VariableContinuity = [];
      props.CustomProperties = struct();
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
      % which made parsing a wide timetable's arguments O(numArgs^2).
      dataArgs = {};
      dataNames = {};
      nArgNames = length(argNames);
      k = 1;
      while k <= length(args)
        option = isConstructorOption(obj, args{k});
        if option.isOption
          if k == length(args)
            error('Nelson:timetable:nameValuePair:nameValueArgumentFollowedByA', 'Name-value argument must be followed by a value.');
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
      opts.DimensionNames = {};
      opts.Description = [];
      opts.UserData = [];
      opts.VariableDescriptions = [];
      opts.VariableUnits = [];
      opts.VariableContinuity = [];
      opts.Size = [];
      opts.VariableTypes = [];
      opts.RowTimes = [];
      opts.SampleRate = [];
      opts.TimeStep = [];
      opts.StartTime = [];
      opts.Events = [];
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
      if ~ischar(value) || ~isrow(value)
        return
      end
      optionNames = {'VariableNames', 'DimensionNames', 'Description', 'UserData', ...
        'VariableDescriptions', 'VariableUnits', 'VariableContinuity', 'Size', ...
        'VariableTypes', 'RowTimes', 'SampleRate', 'TimeStep', 'StartTime', 'Events'};
      idx = find(strcmpi(value, optionNames));
      if ~isempty(idx)
        option.isOption = true;
        option.name = optionNames{idx(1)};
      end
    end

    function [rowTimes, dataArgs, dataNames, rowTimeName] = resolveRowTimes(obj, dataArgs, dataNames, opts)
      rowTimeName = '';
      if ~isempty(opts.RowTimes)
        rowTimes = opts.RowTimes;
        return
      end
      nRows = dataHeight(obj, dataArgs);
      if ~isempty(opts.SampleRate) || ~isempty(opts.TimeStep)
        rowTimes = timetableRegularRowTimes(nRows, opts);
        return
      end
      if isempty(dataArgs)
        error('Nelson:timetable:invalidSyntax', 'Timetable row times must be specified.');
      end
      rowTimes = dataArgs{1};
      if ~isTimeVector(obj, rowTimes)
        error('Nelson:timetable:invalidRowTimes', 'Timetable row times must be datetime or duration values.');
      end
      if ~isempty(dataNames)
        rowTimeName = dataNames{1};
      end
      dataArgs(1) = [];
      dataNames(1) = [];
    end

    function obj = preallocate(obj, opts)
      if isempty(opts.Size) || isempty(opts.VariableTypes)
        error('Nelson:timetable:invalidSyntax:sizeVariableTypesSpecifiedTogether', 'Size and VariableTypes must be specified together.');
      end
      if ~isnumeric(opts.Size) || length(opts.Size) ~= 2
        error('Nelson:timetable:invalidValue:sizeATwoElementNumericVector', 'Size must be a two-element numeric vector.');
      end
      if isempty(opts.RowTimes) && isempty(opts.SampleRate) && isempty(opts.TimeStep)
        error('Nelson:timetable:invalidSyntax:preallocatedTimetablesRequireRowTimesSampleRate', 'Preallocated timetables require row times, SampleRate, or TimeStep.');
      end
      nRows = opts.Size(1);
      if ~isempty(opts.RowTimes)
        rowTimes = opts.RowTimes;
      else
        rowTimes = timetableRegularRowTimes(nRows, opts);
      end
      T = timetableCreateTableStorage('Size', opts.Size, 'VariableTypes', opts.VariableTypes);
      if ~isempty(opts.VariableNames)
        T.Properties.VariableNames = checkedVariableNames(obj, opts.VariableNames);
      end
      obj.Table = T;
      obj = applyConstructorDimensionNames(obj, opts);
      obj.Properties = mergeTableProperties(obj, obj.Properties, T.Properties);
      obj.Properties.RowTimes = columnTimes(obj, rowTimes);
      obj = applyConstructorProperties(obj, opts);
      obj = validateAndRefresh(obj);
    end

    function obj = applyConstructorDimensionNames(obj, opts)
      % Dimension names are set before the variable names so that the
      % duplicate check only sees the final names.
      if ~isempty(opts.DimensionNames)
        obj.Properties.DimensionNames = checkedDimensionNames(obj, opts.DimensionNames);
      end
    end

    function obj = applyConstructorProperties(obj, opts)
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
      if ~isempty(opts.Events)
        obj.Properties.Events = timetableCheckedEvents(opts.Events, obj.Properties.RowTimes);
      end
    end

    function obj = validateAndRefresh(obj)
      st = struct(obj);
      tableStruct = refreshTableProperties(obj, st.Table);
      obj.Table = tableStruct;
      obj.Properties = mergeTableProperties(obj, obj.Properties, tableStruct.Properties);
      obj.Properties.RowTimes = columnTimes(obj, obj.Properties.RowTimes);
      if ~isTimeVector(obj, obj.Properties.RowTimes)
        error('Nelson:timetable:invalidRowTimes', 'Timetable row times must be datetime or duration values.');
      end
      if tableWidthFromStruct(obj, tableStruct) > 0 ...
          && timeLength(obj, obj.Properties.RowTimes) ~= tableHeightFromStruct(obj, tableStruct)
        error('Nelson:timetable:incorrectNumberOfRowTimes', _('RowTimes property must contain one time for each row in the timetable.'));
      end
      if length(obj.Properties.DimensionNames) ~= 2
        error('Nelson:timetable:invalidValue:dimensionNamesContainTwoNames', 'DimensionNames must contain two names.');
      end
      obj.Properties.StartTime = [];
      obj.Properties.TimeStep = [];
      obj.Properties.SampleRate = [];
      if ~isempty(obj.Properties.RowTimes)
        obj.Properties.StartTime = obj.Properties.RowTimes(1);
      end
      obj.Version = 1;
    end

    function T = refreshTableProperties(obj, T)
      props = T.Properties;
      props.DimensionNames = {'Row', 'Variables'};
      props.RowNames = {};
      T.Properties = props;
      dataNames = fieldnames(T.data);
      names = T.Properties.VariableNames;
      % Read the variables by position. T.data.(names{k}) and isfield per name
      % are field scans, so these per-variable loops would be O(numVars^2).
      if length(dataNames) == length(names) && ~isempty(names)
        if ~all(ismember(names, dataNames))
          T.data = __tableStructFromCell__(struct2cell(T.data), names(:));
          dataNames = fieldnames(T.data);
        end
      end
      n = length(names);
      if n > 0 && isequal(sort(dataNames(:)), sort(names(:)))
        vals = struct2cell(T.data);
        [~, loc] = ismember(names, dataNames);
        types = strings(1, n);
        for k = 1:n
          types(k) = class(vals{loc(k)});
        end
        T.Properties.VariableTypes = types;
      end
    end

    function props = mergeTableProperties(obj, props, tableProps)
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
      props.VariableNames = tableProps.VariableNames;
      props.VariableTypes = tableProps.VariableTypes;
      props.VariableDescriptions = tableProps.VariableDescriptions;
      props.VariableUnits = tableProps.VariableUnits;
      props.VariableContinuity = tableProps.VariableContinuity;
      props.DimensionNames = toCellstrRow(obj, props.DimensionNames);
      props.CustomProperties = tableProps.CustomProperties;
    end

    function R = dotSubsref(obj, sref)
      field = sref(1).subs;
      if strcmp(field, 'Properties')
        if length(sref) > 1 && strcmp(sref(2).type, '.')
          R = obj.Properties.(sref(2).subs);
          if length(sref) > 2
            R = subsref(R, sref(3:end));
          end
          return
        end
        R = obj.Properties;
      elseif strcmp(field, obj.Properties.DimensionNames{1})
        R = obj.Properties.RowTimes;
      elseif strcmp(field, obj.Properties.DimensionNames{2})
        T = tableObject(obj);
        R = T{:, :};
      elseif any(strcmp(obj.Properties.VariableNames, field))
        % T.(field) (read or write) would resolve Data or Version to hidden properties
        st = tableStructValue(obj);
        R = st.data.(field);
      elseif isfield(obj.Properties, field)
        error('Nelson:table:illegalPropertyReference', _('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), field, field);
      else
        error('Nelson:timetable:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), field);
      end
      if length(sref) > 1
        R = subsref(R, timetableVariableRowSubscript(obj.Properties, field, sref(2:end)));
      end
    end

    function R = braceSubsref(obj, sref)
      if length(sref(1).subs) ~= 2
        error('Nelson:timetable:invalidSubscript', '%s indexing requires row and variable subscripts.', 'Brace');
      end
      rows = timetableResolveRows(obj.Properties.RowTimes, sref(1).subs{1});
      T = tableObject(obj);
      R = T{rows, sref(1).subs{2}};
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function R = parenSubsref(obj, sref)
      if length(sref(1).subs) ~= 2
        error('Nelson:timetable:invalidSubscript', '%s indexing requires row and variable subscripts.', 'Parentheses');
      end
      rows = timetableResolveRows(obj.Properties.RowTimes, sref(1).subs{1});
      T = tableObject(obj);
      T = T(rows, sref(1).subs{2});
      R = timetable.fromTableAndTimes(T, timeRows(obj, obj.Properties.RowTimes, rows), obj.Properties);
      if length(sref) > 1
        R = subsref(R, sref(2:end));
      end
    end

    function obj = dotSubsasgn(obj, sasgn, value)
      if length(sasgn) == 2 && any(strcmp(sasgn(2).type, {'()', '{}'}))
        obj = dotIndexSubsasgn(obj, sasgn, value);
        return
      end
      if length(sasgn) ~= 1
        error('Nelson:timetable:unsupportedAssignment', 'Unsupported %s assignment.', 'timetable');
      end
      field = sasgn(1).subs;
      if strcmp(field, 'Properties')
        obj.Properties = timetableAssignedProperties(obj.Properties, value);
      elseif strcmp(field, obj.Properties.DimensionNames{1})
        if isTimeVector(obj, value) && timeLength(obj, value) ~= timeLength(obj, obj.Properties.RowTimes)
          error('Nelson:table:dimagree:valueAssignmentNumberRowsTable', 'Value assignment must have the same number of rows as the table.');
        end
        obj.Properties.RowTimes = columnTimes(obj, value);
      elseif any(strcmp(obj.Properties.VariableNames, field)) || isvarname(field)
        obj.Table = struct(subsasgn(tableObject(obj), sasgn, value));
      else
        error('Nelson:timetable:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), field);
      end
      obj = validateAndRefresh(obj);
    end

    function obj = dotIndexSubsasgn(obj, sasgn, value)
      % TT.name(indices) = value, TT.name{indices} = value: the table assigns
      % a variable (new or not), may add rows (given or missing row times).
      field = sasgn(1).subs;
      if strcmp(field, 'Properties') || any(strcmp(obj.Properties.DimensionNames, field)) ...
          || ~(any(strcmp(obj.Properties.VariableNames, field)) || isvarname(field))
        obj = dotSubsasgn(obj, sasgn(1), subsasgn(subsref(obj, sasgn(1)), sasgn(2), value));
        return
      end
      [sasgn(2), newTimes] = timetableVariableRowSubscript(obj.Properties, field, sasgn(2), true);
      T = subsasgn(tableObject(obj), sasgn, value);
      obj.Table = struct(T);
      obj.Properties.RowTimes = timetableGrowRowTimes(obj.Properties.RowTimes, height(T), newTimes);
      obj = validateAndRefresh(obj);
    end

    function obj = parenSubsasgn(obj, sasgn, value)
      if length(sasgn(1).subs) ~= 2
        error('Nelson:timetable:invalidSubscript', '%s indexing requires row and variable subscripts.', 'Parentheses');
      end
      isDeletion = isa(value, 'double') && ~issparse(value) && isreal(value) && isequal(size(value), [0 0]); % only [] deletes
      [rows, newTimes] = timetableResolveRows(obj.Properties.RowTimes, sasgn(1).subs{1}, ~isDeletion);
      vars = sasgn(1).subs{2};
      if isDeletion
        T = tableObject(obj);
        if ischar(sasgn(1).subs{1}) && strcmp(sasgn(1).subs{1}, ':')
          T(:, vars) = [];
        else
          T(rows, vars) = [];
        end
        obj.Table = struct(T);
        if ischar(vars) && strcmp(vars, ':')
          keep = true(timeLength(obj, obj.Properties.RowTimes), 1);
          keep(rows) = false;
          obj.Properties.RowTimes = timeRows(obj, obj.Properties.RowTimes, keep);
        end
      else
        T = tableObject(obj);
        if istimetable(value), value = tableObject(value); end % its row times are not assigned
        T(rows, vars) = value; % a table or a cell array: checked by the table
        obj.Table = struct(T);
        obj.Properties.RowTimes = timetableGrowRowTimes(obj.Properties.RowTimes, height(T), newTimes);
      end
      obj = validateAndRefresh(obj);
    end

    function tf = isTimeVector(obj, value)
      tf = isa(value, 'datetime') || isa(value, 'duration');
    end

    function value = columnTimes(obj, value)
      if isTimeVector(obj, value)
        value = value(:);
      elseif isrow(value)
        value = value(:);
      end
    end

    function n = timeLength(obj, value)
      if isa(value, 'datetime')
        n = numel(datenum(value));
      elseif isa(value, 'duration')
        n = numel(seconds(value));
      else
        n = length(value);
      end
    end

    function selected = timeRows(obj, value, rows)
      if islogical(rows)
        rows = find(rows);
      end
      try
        selected = value(rows);
        selected = columnTimes(obj, selected);
        return
      catch
      end
      rows = rows(:);
      if isempty(rows)
        selected = value([]);
        return
      end
      selected = value(rows(1));
      selected = selected(:);
      for k = 2:length(rows)
        selected(end + 1, 1) = value(rows(k));
      end
    end

    function nRows = dataHeight(obj, dataArgs)
      if isempty(dataArgs)
        nRows = 0;
      else
        nRows = size(dataArgs{1}, 1);
      end
    end

    function st = tableStructValue(obj)
      raw = struct(obj);
      st = raw.Table;
      if istable(st)
        st = struct(st);
      end
    end

    function T = tableObject(obj)
      T = table.fromStruct(tableStructValue(obj));
    end

    function dispTimetableFull(obj)
      st = tableStructValue(obj);
      varNames = st.Properties.VariableNames;
      rowTimes = obj.Properties.RowTimes;
      numCols = tableWidthFromStruct(obj, st);
      numRows = height(obj);
      strs = strings(numRows + 1, numCols + 1);
      strs{1, 1} = obj.Properties.DimensionNames{1};
      for j = 1:numCols
        strs{1, j + 1} = varNames{j};
      end
      if isa(rowTimes, 'datetime')
        % One format for the whole column: a midnight row keeps its time
        % when another row has one.
        strs(2:end, 1) = table.displayColumnText(rowTimes, 1:numRows);
      else
        for i = 1:numRows
          strs{i + 1, 1} = timetableTimeCellText(obj, rowTimes, i);
        end
      end
      for j = 1:numCols
        strs(2:end, j + 1) = table.displayColumnText(st.data.(varNames{j}), 1:numRows);
      end
      table.formatTableDisplay(timetableEventColumn(strs, obj.Properties.Events, rowTimes), true);
    end

    function text = timetableTimeCellText(obj, times, index)
      value = timeRows(obj, times, index);
      if isa(value, 'duration')
        text = timetableDurationCellText(value);
      else
        text = table.displayCellText(value);
      end
    end

    function n = tableWidth(obj)
      n = tableWidthFromStruct(obj, tableStructValue(obj));
    end

    function n = tableWidthFromStruct(obj, st)
      n = length(st.Properties.VariableNames);
    end

    function n = tableHeightFromStruct(obj, st)
      if isempty(st.Properties.VariableNames)
        n = 0;
      else
        n = size(st.data.(st.Properties.VariableNames{1}), 1);
      end
    end
  end
end
%=============================================================================
