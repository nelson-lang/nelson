%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = empty(varargin)
  % timetable.empty, timetable.empty(0, n), timetable.empty(m, 0) or
  % timetable.empty([0 n]): an empty timetable of that size, with datetime
  % row times (NaT) and n double variables Var1 ... Varn.
  sz = size(double.empty(varargin{:}));
  if length(sz) > 2
    error('Nelson:timetable:emptyMustBeTwoDims', _('A timetable must be two-dimensional.'));
  end
  columns = repmat({zeros(0, 0)}, 1, sz(2));
  obj = timetable(NaT(sz(1), 1), columns{:});
end
%=============================================================================
