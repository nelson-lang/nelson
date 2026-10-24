%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef eventtable < timetable
  % An eventtable is a timetable whose row times are event times. The
  % timetable storage is inherited unchanged; the names of the variables
  % holding the event labels, lengths and ends live in EventVariables and
  % are shown at the top of Properties.
  properties (Hidden)
    EventVariables
  end

  methods
    function obj = eventtable(varargin)
      obj@timetable();
      obj.EventVariables = eventtableNoVariables();
      if nargin == 0
        return
      end
      [TT, eventVariables] = eventtableConstruct(varargin);
      obj = eventtable.assign(obj, TT, eventVariables);
    end

    function R = subsref(obj, sref)
      switch sref(1).type
        case '.'
          name = sref(1).subs;
          storage = obj.Table;
          if strcmp(name, 'Properties')
            R = eventtablePropertiesView(obj.Properties, obj.EventVariables);
          elseif isscalar(sref) && isstruct(storage) && any(strcmp(name, storage.Properties.VariableNames))
            % the storage holds the variables as the timetable subsref reads them
            R = storage.data.(name);
          else
            R = subsref@timetable(obj, sref);
            return
          end
          if length(sref) > 1
            R = subsref(R, sref(2:end));
          end
        case '()'
          R = subsref@timetable(obj, sref(1));
          R = eventtable.fromTimetable(R, obj.EventVariables);
          if length(sref) > 1
            R = subsref(R, sref(2:end));
          end
        otherwise
          R = subsref@timetable(obj, sref);
      end
    end

    function obj = subsasgn(obj, sasgn, value)
      if strcmp(sasgn(1).type, '.') && strcmp(sasgn(1).subs, 'Properties')
        current = eventtablePropertiesView(obj.Properties, obj.EventVariables);
        if length(sasgn) > 1
          value = subsasgn(current, sasgn(2:end), value);
        end
        [value, requested] = eventtableSplitProperties(value, obj.Properties);
        obj = subsasgn@timetable(obj, struct('type', '.', 'subs', 'Properties'), value);
        st = struct(obj);
        obj.EventVariables = eventtableCheckVariables(requested, current, st.Table.data, ...
          obj.Properties.VariableNames, obj.Properties.RowTimes);
        return
      end
      obj = subsasgn@timetable(obj, sasgn, value);
      obj.EventVariables = eventtablePruneVariables(obj.EventVariables, obj.Properties.VariableNames);
    end

    function tf = isequal(obj, varargin)
      % Dispatched when any argument is an eventtable, not only the first.
      tf = isa(obj, 'eventtable');
      if ~tf
        return
      end
      for k = 1:length(varargin)
        other = varargin{k};
        tf = isa(other, 'eventtable') ...
          && isequal(eventtablePruneVariables(obj.EventVariables, obj.Properties.VariableNames), ...
          eventtablePruneVariables(other.EventVariables, other.Properties.VariableNames));
        if tf
          tf = isequal@timetable(obj, other);
        end
        if ~tf
          return
        end
      end
    end
  end

  methods (Static)
    obj = empty(varargin)
  end

  methods (Static, Hidden)
    function E = fromTimetable(TT, eventVariables)
      % Wraps the result of a timetable operation on E as an eventtable.
      E = eventtable.assign(eventtable(), TT, eventVariables);
    end

    function TT = toTimetable(E)
      st = struct(E);
      TT = timetable.fromTableAndTimes(st.Table, st.Properties.RowTimes, st.Properties);
    end

    function vars = variables(E)
      vars = E.EventVariables;
    end

    function R = applyTimetableFunction(fun, varargin)
      % A timetable function called with an eventtable argument, whatever
      % its position, returns an eventtable.
      R = eventtableMath(fun, varargin{:});
    end
  end

  methods (Static, Access = private)
    function obj = assign(obj, TT, eventVariables)
      % The table storage first: the Properties setter then only stores
      % the per-variable metadata, already in the storage.
      st = struct(TT);
      obj.Table = st.Table;
      obj.Version = st.Version;
      obj.Properties = st.Properties;
      obj.EventVariables = eventtablePruneVariables(eventVariables, st.Properties.VariableNames);
    end
  end
end
%=============================================================================
