%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef labels
  % Timetable row subscript that selects rows by their row time values:
  % TT(labels(3), :) is the row at time 3, TT(3, :) the third row.
  properties (Hidden)
    Values = []
  end
  methods
    function obj = labels(values)
      narginchk(1, 1);
      if ~(isa(values, 'datetime') || isa(values, 'duration') || isa(values, 'double') || isa(values, 'single'))
        error('Nelson:labels:invalidLabels', 'Labels must be datetime, duration, double or single values.');
      end
      obj.Values = values;
    end

    function disp(obj)
      fprintf('  labels subscript for %s row times\n', class(obj.Values));
    end
  end
  methods (Static, Hidden)
    function values = values(obj)
      state = builtin('struct', obj);
      values = state.Values;
    end
  end
end
%=============================================================================
