%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = eventtableMath(fun, varargin)
  % Applies the timetable operation fun; a timetable result is an
  % eventtable whose event variables are the first ones set among the
  % eventtable inputs.
  eventVariables = eventtableNoVariables();
  fields = fieldnames(eventVariables);
  for k = 1:length(varargin)
    if isa(varargin{k}, 'eventtable')
      vars = eventtable.variables(varargin{k});
      for f = 1:length(fields)
        if isempty(eventVariables.(fields{f}))
          eventVariables.(fields{f}) = vars.(fields{f});
        end
      end
      varargin{k} = eventtable.toTimetable(varargin{k});
    end
  end
  R = fun(varargin{:});
  if istimetable(R)
    R = eventtable.fromTimetable(R, eventVariables);
  end
end
%=============================================================================
